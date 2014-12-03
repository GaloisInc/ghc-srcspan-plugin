{-# LANGUAGE OverloadedStrings #-}
module Imp where

import Prelude hiding (and, or, not)

import           Control.Monad.Writer
import           Data.String
import           GHC.Plugins.SrcSpan
import qualified GhcPlugins           as GHC

type Imp a = Writer Block a

runImp :: Imp a -> Block
runImp = execWriter

type Var = String

data Lit = Integer Integer
         | Bool Bool
         deriving (Show)

data Expr = Var Var
          | Lit Lit
          | And Expr Expr
          | Or Expr Expr
          | Not Expr
          | Eq Expr Expr
          | Gt Expr Expr
          | Lt Expr Expr
          | Add Expr Expr
          | Sub Expr Expr
          deriving (Show)

type Block = [Stmt]

data Stmt = Assign Var Expr
          | IfTE Expr Block Block
          | While Expr Block
          | Assert Expr
          | SrcSpan GHC.RealSrcSpan
          deriving (Show)

true, false :: Expr
true  = Lit (Bool True)
false = Lit (Bool False)

instance Num Expr where
  fromInteger x = Lit (Integer x)
  (+) = Add
  (-) = Sub

instance IsString Expr where
  fromString = Var

and, or, (=?), (>?), (<?) :: Expr -> Expr -> Expr
and = And
or = Or
(=?) = Eq
(>?) = Gt
(<?) = Lt

not :: Expr -> Expr
not = Not

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


-----------------------------------------------------------------------

sum10 :: Imp ()
sum10 = do
  let n = "n"
  let r = "r"
  n =: 0
  r =: 0
  while (n <? 11) $ do
    r =: r + n
    n =: n + 1
  assert (r =? 55)
