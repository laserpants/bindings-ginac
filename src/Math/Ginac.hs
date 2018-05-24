module Math.Ginac 
  ( Expr
  , Symbol
  , abs
  , add
  , diff
  , diffn
  , div
  , factorial
  , mul
  , neg
  , newSymbol
  , num
  , pow
  , rational
  , signum
  , sqrt
  , staticSymbol
  , subs
  , subsInt
  , toString
  , var
  ) where

import Control.Monad
import Data.Ratio
import Foreign
import Foreign.C.String
import Math.Ginac.FFI
import Math.Ginac.FFI.ForeignPtr
import System.IO.Unsafe

import Prelude hiding ( abs, div, signum, sqrt )

newtype Expr = Ex GinacExPtr
newtype Symbol = Sy GinacSymbolPtr

unsafeExpr :: IO GinacExPtr -> Expr
unsafeExpr = Ex . unsafePerformIO

expr :: IO (Ptr GinacEx) -> Expr
expr io = unsafeExpr (io >>= newForeignPtr ginac_ex_free_fun)

num :: Int -> Expr
num = expr . ginac_ex_new_from_int

var :: Symbol -> Expr
var (Sy ptr) = expr (withForeignPtr ptr ginac_ex_new_from_basic)

binop :: (Ptr a -> Ptr b -> IO c) -> ForeignPtr a -> ForeignPtr b -> IO c
binop op p q = withForeignPtr p (withForeignPtr q . op)

add :: Expr -> Expr -> Expr
add (Ex p) (Ex q) = expr (binop ginac_add p q)

mul :: Expr -> Expr -> Expr
mul (Ex p) (Ex q) = expr (binop ginac_mul p q)

div :: Expr -> Expr -> Expr
div (Ex p) (Ex q) = expr (binop ginac_div p q)

pow :: Expr -> Expr -> Expr
pow (Ex p) (Ex q) = expr (binop ginac_pow p q)

neg :: Expr -> Expr
neg (Ex ptr) = expr (withForeignPtr ptr ginac_ex_neg)

abs :: Expr -> Expr
abs (Ex ptr) = expr (withForeignPtr ptr ginac_ex_abs)

signum :: Expr -> Expr
signum (Ex ptr) = expr (withForeignPtr ptr ginac_ex_signum)

diff :: Expr -> Symbol -> Expr
diff (Ex p) (Sy q) = expr (binop (ginac_diff 1) p q)

diffn :: Int -> Expr -> Symbol -> Expr
diffn nth (Ex p) (Sy q) = expr (binop (ginac_diff nth) p q)

factorial :: Int -> Expr
factorial = expr . ginac_factorial

sqrt :: Expr -> Expr
sqrt (Ex ptr) = expr (withForeignPtr ptr ginac_ex_sqrt)

subs :: Expr -> Symbol -> Expr -> Expr
subs (Ex p) (Sy q) (Ex r) = expr (withForeignPtr p ptr) where
    ptr ex = withForeignPtr q (withForeignPtr r . ginac_ex_subs ex)

subsInt :: Expr -> Symbol -> Int -> Expr
subsInt (Ex p) (Sy q) i = expr ptr where
    ptr = withForeignPtr p (withForeignPtr q . ginac_ex_subs_int i)

rational :: Rational -> Expr
rational r = div (num n) (num d) where
    n = fromInteger (numerator r)
    d = fromInteger (denominator r)

newSymbol :: String -> IO Symbol
newSymbol name = liftM Sy ptr where
    ptr = withCString name ginac_symbol_new 
      >>= newForeignPtr ginac_basic_free_fun

staticSymbol :: IO (Ptr GinacSymbol)
staticSymbol = ginac_symbol_static

toString :: Expr -> String
toString (Ex ptr) = unsafePerformIO io where
    io = withForeignPtr ptr ginac_ex_to_str
     >>= peekCString
