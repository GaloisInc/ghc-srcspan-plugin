{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Imp where

import Prelude hiding (and, or, not)

import           Control.Monad.Writer
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.String
import           Text.Printf

newtype Imp a = Imp (Writer Block a)
  deriving (Monad, MonadWriter Block)

runImp :: Imp a -> Block
runImp (Imp a) = execWriter a

type Var = String

data Lit = Integer Integer
         | Bool Bool
         deriving (Show)

data Expr = Var Var
          | Lit Lit
          | Eq Expr Expr
          | Lt Expr Expr
          | Add Expr Expr
          deriving (Show)

type Block = [Stmt]

data Stmt = Assign Var Expr
          | IfTE Expr Block Block
          | While Expr Block
          | Assert Expr
          | SetLocation ImpSrcSpan
          deriving (Show)

data ImpSrcSpan = ImpSrcSpan FilePath Int Int Int Int

instance Show ImpSrcSpan where
  show (ImpSrcSpan f l1 c1 l2 c2)
    = printf "\"%s:(%d,%d)-(%d,%d)\"" f l1 c1 l2 c2

noSrcSpan :: ImpSrcSpan
noSrcSpan = ImpSrcSpan "<location unknown>" 0 0 0 0

errorLoc :: ImpSrcSpan -> String -> a
errorLoc loc msg = error $ printf "%s: %s" (show loc) msg

data Value = IntVal Integer
           | BoolVal Bool
           | Void
           deriving (Show)

eval :: Block -> Map Var Value
eval = fst . foldl' go (Map.empty, noSrcSpan)
  where
  go :: (Map Var Value, ImpSrcSpan) -> Stmt -> (Map Var Value, ImpSrcSpan)
  go (env, loc) stmt = case stmt of
    Assign v e -> (Map.insert v (evalExpr (env, loc) e) env, loc)
    SetLocation loc' -> (env, loc')
    IfTE b t f -> case evalExpr (env, loc) b of
      BoolVal True  -> foldl' go (env, loc) t
      BoolVal False -> foldl' go (env, loc) f
      v             -> errorLoc loc $ printf "expected boolean value, got: %s" (show v)
    While b s -> case evalExpr (env, loc) b of
      BoolVal True  -> foldl' go (env, loc) (s ++ [While b s])
      BoolVal False -> (env, loc)
      v             -> errorLoc loc $ printf "expected boolean value, got: %s" (show v)
    Assert e -> case evalExpr (env, loc) e of
      BoolVal True  -> (env, loc)
      BoolVal False -> errorLoc loc $ printf "assertion failed: %s" (show e)
      v             -> errorLoc loc $ printf "expected boolean value, got: %s" (show v)

  evalExpr :: (Map Var Value, ImpSrcSpan) -> Expr -> Value
  evalExpr (env, loc) expr = case expr of
    Var v -> Map.findWithDefault (errorLoc loc $ printf "undefined variable: %s" v)
                                 v env
    Lit (Integer i) -> IntVal i
    Lit (Bool b)    -> BoolVal b
    Eq e1 e2        -> case (evalExpr (env, loc) e1, evalExpr (env, loc) e2) of
      (IntVal i1, IntVal i2) -> BoolVal (i1 == i2)
      (BoolVal b1, BoolVal b2) -> BoolVal (b1 == b2)
      (v1, v2) -> errorLoc loc $ printf "invalid operands to Lt: (%s, %s)" (show v1) (show v2)
    Lt e1 e2        -> case (evalExpr (env, loc) e1, evalExpr (env, loc) e2) of
      (IntVal i1, IntVal i2) -> BoolVal (i1 < i2)
      (v1, v2) -> errorLoc loc $ printf "invalid operands to Lt: (%s, %s)" (show v1) (show v2)
    Add e1 e2        -> case (evalExpr (env, loc) e1, evalExpr (env, loc) e2) of
      (IntVal i1, IntVal i2) -> IntVal (i1 + i2)
      (v1, v2) -> errorLoc loc $ printf "invalid operands to Add: (%s, %s)" (show v1) (show v2)
    
    

true, false :: Expr
true  = Lit (Bool True)
false = Lit (Bool False)

instance Num Expr where
  fromInteger x = Lit (Integer x)
  (+) = Add

instance IsString Expr where
  fromString = Var

(=?), (<?) :: Expr -> Expr -> Expr
(=?) = Eq
(<?) = Lt

var :: Var -> Expr
var = Var

infixr 0 =:
(=:) :: Expr -> Expr -> Imp ()
Var x =: e = tell [Assign x e]

ifte :: Expr -> Imp () -> Imp () -> Imp ()
ifte b t f = tell [IfTE b (runImp t) (runImp f)]

while :: Expr -> Imp () -> Imp ()
while b s = tell [While b (runImp s)]

assert :: Expr -> Imp ()
assert b = tell [Assert b]

makeLocation :: FilePath -> Int -> Int -> Int -> Int -> ImpSrcSpan
makeLocation = ImpSrcSpan

setLocation :: ImpSrcSpan -> Imp ()
setLocation loc = tell [SetLocation loc]

withLocation :: ImpSrcSpan -> Imp a -> Imp a
withLocation loc doThis = setLocation loc >> doThis
