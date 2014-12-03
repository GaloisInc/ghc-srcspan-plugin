{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Imp where

import Prelude hiding (and, or, not)

import           Control.Monad.Writer
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
          | SetLocation SrcSpan
          deriving (Show)

data SrcSpan = SrcSpan FilePath Int Int Int Int

instance Show SrcSpan where
  show (SrcSpan f l1 c1 l2 c2)
    = printf "\"%s:(%d,%d)-(%d,%d)\"" f l1 c1 l2 c2

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

makeLocation :: FilePath -> Int -> Int -> Int -> Int -> SrcSpan
makeLocation = SrcSpan

setLocation :: SrcSpan -> Imp ()
setLocation loc = tell [SetLocation loc]

withLocation :: SrcSpan -> Imp a -> Imp a
withLocation loc doThis = setLocation loc >> doThis
