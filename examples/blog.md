# (Ab)using Compiler Plugins to Improve Embedded DSLs

Embedded DSLs are a bit of a double-edged sword. They have a low start-up cost because you can defer a lot of work to the host language, but producing good error messages can be challenging. People often talk about the quality of *type errors* produced by the host language, but I'm going to focus instead on producing better *runtime errors*.

## A Simple Toy Language

Here's a fragment of a simple imperative language.

```haskell
data Lit = Integer Integer
         deriving (Show)

data Expr = Var String
          | Lit Lit
          | Eq Expr Expr
          | Lt Expr Expr
          | Add Expr Expr
          deriving (Show)

data Stmt = Assign String Expr
          | While Expr [Stmt]
          | Assert Expr
          deriving (Show)
```

With judicious use of smart constructors, we can build a nice embedded DSL for our language, turning

```haskell
sum10 :: Imp ()
sum10 = do
  n <- local 0
  r <- local 0
  while (n <? 11) $ do
    r =: r + n
    n =: n + 1
  assert (r =? 54)
```

into

```haskell
λ> runImp sum10
[ Assign "local0" (Lit (Integer 0))
, Assign "local1" (Lit (Integer 0))
, While
    (Lt (Var "local0") (Lit (Integer 11)))
    [ Assign "local1" (Add (Var "local1") (Var "local0"))
    , Assign "local0" (Add (Var "local0") (Lit (Integer 1)))
    ]
, Assert (Eq (Var "local1") (Lit (Integer 54)))
]
```

But when we actually run the program, we get

```haskell
λ> eval $ runImp sum10
error: assertion failed: Eq (Var "local1") (Lit (Integer 54))
```

which is not so great. I like my error messages to include a source location so I know where to start looking. Unfortunately there's no way for a Haskell function to know where it was called, and for good reason as that would destroy purity.

As an alternative, we could use a pre-processor to transform the original Haskell code by adding explicit references to the source locations. But that's a bit unsatisfactory because now the code we write is no longer the same code GHC sees, which means that errors thrown by GHC will refer to incorrect locations. Luckily for us, GHC includes support for *compiler plugins* so users can implement their own optimization passes. So, today we're going to implement an optimization pass that optimizes usability rather than performance.

## Strategy

GHC allows users to write optimization passes over [Core], the first of a few intermediate representations used by GHC. Core is a simple language with just a handful of data constructors, essentially [^actually]

```haskell
data CoreExpr
  = Var	  Id
  | Lit   Literal
  | App   CoreExpr CoreExpr
  | Lam   Id CoreExpr
  | Let   CoreBind CoreExpr
  | Case  CoreExpr Id Type [(AltCon, [Id], CoreExpr)]
  | Type  Type
  | Tick  Tickish CoreExpr
```

[Core]: https://downloads.haskell.org/~ghc/latest/docs/html/libraries/ghc/CoreSyn.html#t:Expr
[^actually]: The actual definition has two extra constructors and a type parameter which I've instantiated with `Id`, but this is not particularly relevant to our use-case.

This makes our life a whole lot easier since we don't have to consider the entire surface area of Haskell's syntax when we write our plugin.

Our goal is to write a Core transformation that will insert calls to a `setLocation` action in our monadic DSL, transforming the original Haskell code into something like

```haskell
sum10 :: Imp ()
sum10 = do
  setLocation <line 3>
  n <- local 0
  setLocation <line 4>
  r <- local 0
  setLocation <line 5>
  while (n <? 11) $ do
    setLocation <line 6>
    r =: r + n
    setLocation <line 7>
    n =: n + 1
  setLocation <line 8>
  assert (r =? 54)
```

This isn't perfect as our language will only know about source locations with statement-level granularity, but the upside is that the changes to the language are minimal. We can just add another `Stmt` constructor that tells the interpreter to update the current location.

To write this transformation we need to know three things:

1. Where to insert the annotations?
2. How to insert the annotations?
3. How to get the source locations from GHC?

## Useful API Functions

GHC is written as a library with a vast API, so let's first pick out and describe a few functions that we'll need to use. I'm going to take some artistic license with the types of these API functions in order to hide some of the necessary plumbing. A complete and running version of the plugin can be found [here].

[here]: https://github.com/GaloisInc/ghc-srcspan-plugin/blob/master/examples/ImpPluginExplicit.hs

### Deconstructing Expressions and Types

```haskell
exprType            :: CoreExpr -> Type
splitTyConApp_maybe :: Type -> Maybe (TyCon, [Type])
```

`exprType` queries an expression for its type. `splitTyConApp_maybe` attempts to split a type into a type constructor and its arguments, e.g.

```haskell
splitTyConApp_maybe <Imp String> = Just (<Imp>, [<String>])
```

### Building Core Expressions

```haskell
mkCoreApps   :: CoreExpr -> [CoreExpr] -> CoreExpr
mkStringExpr :: String -> CoreExpr
mkIntExpr    :: Integer -> CoreExpr
```

`mkCoreApps` constructs a sequence of nested applications, e.g.

```haskell
mkCoreApps <map> [<f>, <xs>] = App (App <map> <f>) <xs>
```

`mkStringExpr` and `mkIntExpr` construct expressions corresponding to `String` (resp. `Integer`) literals.

### Library functions from our DSL

We'll also need to define two more functions in our DSL for our code-generator to target.

```haskell
makeLocation :: FilePath
             -> Int -> Int -- the starting line/column
             -> Int -> Int -- the ending line/column
             -> ImpSrcSpan
setLocation  :: ImpSrcSpan -> Imp ()
```

`setLocation` just emits a new statement in our DSL that contains the current source location, e.g.

```haskell
data Stmt = ...
          | SetLocation ImpSrcSpan
```

I'm also using a new `ImpSrcSpan` type rather than GHC's `SrcSpan` to emphasize that we can't just embed the `SrcSpan` value directly, we have to reconstruct it at run-time.

## Finding interesting expressions

Since our goal is locations with statement-level granularity, we'll consider any expression with type `Imp a` interesting. Encoding this as a predicate on Core expressions is straightforward, we just use `splitTyConApp_maybe` and check if the
type constructor is `Imp`.

```haskell
isInteresting :: CoreExpr -> Bool
isInteresting expr
  | Just (tc, _) <- splitTyConApp_maybe (exprType expr)
  = tc == <Imp>
  | otherwise
  = False
```


<!-- `splitTyConApp_maybe` attempts to split a `Type` into a pair of a type constructor and its arguments. If it succeeds we just need to check if the type constructor is `Imp`, otherwise we can safely ignore the expression. -->

## Adding the locations

Once we've found an interesting expression, we'll need to annotate it with a source location according to our scheme above. So we need a function

```haskell
annotate :: SrcSpan -> CoreExpr -> CoreExpr
```

that transforms `<expr>` into `(>>) (setLocation <loc>) <expr>`. This turns out to be harder than it looks though! Core doesn't have type-classes -- it passes the dictionaries around explicitly -- which means we need to somehow dig up the `Monad` dictionary for `Imp`. Rather than deal with looking up type-class dictionaries, let's take a slightly different approach and rewrite `<expr>` to `withLocation <loc> <expr>`, where

```haskell
withLocation :: ImpSrcSpan -> Imp a -> Imp a
```

is a new monadic action in our DSL. Now our target code will look something like

```haskell
sum10 :: Imp ()
sum10 = do
  n <- withLocation <line 3> (local 0)
  r <- withLocation <line 4> (local 0)
  withLocation <line 5> $ while (n <? 11) $ do
    r =: withLocation <line 6> (r + n)
    n =: withLocation <line 7> (n + 1)
  withLocation <line 8> (assert (r =? 54))
```

<!-- The next problem is that we can't simply embed the `SrcSpan` that GHC is carrying around into the Core as a literal. We have to instead translate the `SrcSpan` into a `CoreExpr` ourselves. We'll add another function to our DSL -->

<!-- ```haskell -->
<!-- ``` -->

<!-- and write a function to translate a `SrcSpan` into a `CoreExpr` that corresponds to an application of `makeLocation` to the components of the `SrcSpan`. -->

As mentioned above, we can't just embed a `SrcSpan` in the Core, so we'll define a quick helper function that will build a call to `makeLocation`.

```haskell
mkLocExpr :: SrcSpan -> CoreExpr
mkLocExpr src =
  mkCoreApps (Var <makeLocation>)
             [ mkStringExpr (srcSpanFile src)
             , mkIntExpr (srcSpanStartLine src)
             , mkIntExpr (srcSpanStartCol src)
             , mkIntExpr (srcSpanEndLine src)
             , mkIntExpr (srcSpanEndCol src)
             ]
```

<!-- Core is explicitly typed, so we have to instantiate `withLocation`s type parameter ourselves based on `expr`s type. -->

Core is explicitly typed, so when we generate the call to `withLocation` inside `annotate`, we have to take care to instantiate `withLocation`s type parameter correctly.

```haskell
annotate :: SrcSpan -> CoreExpr -> CoreExpr
annotate src expr =
  mkCoreApps (Var <withLocation>) [Type resTy, mkLocExpr src, expr]
  where
  Just (_, [resTy]) = splitTyConApp_maybe $ exprType expr
```

## Getting the locations

I've ignored a somewhat crucial detail so far: GHC strips away the source locations as part of the translation from Haskell to Core! Well, it normally does that anyway... If you load your module into GHCi, or compile with profiling or hpc enabled, GHC will insert `Tick`s in the Core, which contain source locations among other things.

So we need a function

```haskell
tickSpan :: Tickish Id -> SrcSpan
```

to extract the `SrcSpan`. I won't present the implementation here because, frankly, it's just a bunch of plumbing.

## Tying it all together

The last piece of the puzzle is the actual expression transformer, which just needs to traverse the `CoreExpr`s, track the most recent valid `SrcSpan`, and annotate the interesting expressions.

```haskell
addLocationsExpr :: CoreExpr -> CoreExpr
addLocationsExpr = go noSrcSpan
  where
  go ss (Tick t expr) 
    | isGoodSrcSpan (tickSpan t)
    = Tick t (go (tickSpan t) expr)
    | otherwise
    = Tick t (go ss expr)
  go ss e@(App expr arg) 
    | isInteresting e
    = annotate ss (App (go ss expr) (go ss arg))
    | otherwise
    = App (go ss expr) (go ss arg)
  go ss (Lam x expr)
    = Lam x (go ss expr)
  go ss (Let bndr expr)
    = Let (addLocationsBind bndr) (go ss expr)
  go ss (Case expr x t alts)
    = Case (go ss expr) x t (mapM (addLocationsAlt ss) alts)
  go _  expr
    = expr

  addLocationsAlt ss (c, xs, expr)
    = (c, xs, go ss expr)

addLocationsBind :: CoreBind -> CoreBind
addLocationsBind (NonRec b expr)
  = NonRec b (addLocationsExpr expr)
addLocationsBind (Rec binds)
  = Rec [(b, addLocationsExpr expr) | (b, expr) <- binds]
```

We can hook our pass into GHC as a plugin with the following wrapper

```haskell
module ImpPlugin where

import GhcPlugins
import Imp

plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install opts todos = do
  reinitializeGlobals   -- GHC requires it, just do it
  let mypass = CoreDoPluginPass "Add Locations"
               (bindsOnlyPass (return . map addLocationsBind))
  return mypass : todos
```

and enable it at compile-time with `-fplugin=ImpPlugin`. Here are the results of all our hard work

```haskell
λ> runImp sum10
[ SetLocation "ImpDemo.hs:(9,9)-(17,19)"
, SetLocation "ImpDemo.hs:(12,3)-(12,9)"
, Assign "local0" (Lit (Integer 0))
, SetLocation "ImpDemo.hs:(9,9)-(17,19)"
, SetLocation "ImpDemo.hs:(13,3)-(13,9)"
, Assign "local1" (Lit (Integer 0))
, SetLocation "ImpDemo.hs:(9,9)-(17,19)"
, SetLocation "ImpDemo.hs:(14,3)-(16,15)"
, While
    (Lt (Var "local0") (Lit (Integer 11)))
    [ SetLocation "ImpDemo.hs:(14,3)-(16,15)"
    , SetLocation "ImpDemo.hs:(15,5)-(15,15)"
    , Assign "local1" (Add (Var "local1") (Var "local0"))
    , SetLocation "ImpDemo.hs:(16,5)-(16,15)"
    , Assign "local0" (Add (Var "local0") (Lit (Integer 1)))
    ]
, SetLocation "ImpDemo.hs:(17,3)-(17,19)"
, Assert (Eq (Var "local1") (Lit (Integer 54)))
]
```

Wonderful!

You may have noticed that the only pieces of the plugin that were actually specific to `Imp` were finding interesting expressions and annotating them with source locations. So I've extracted the rest into a generic [pass] that you can re-use.

[pass]: http://hackage.haskell.org/package/ghc-srcspan-plugin
