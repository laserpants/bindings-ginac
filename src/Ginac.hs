module Ginac 
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
  , toString
  , var
  ) where

import Control.Monad
import Foreign
import Foreign.C.String
import Ginac.FFI
import Ginac.FFI.ForeignPtr
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

rational :: Rational -> Expr
rational = undefined

newSymbol :: String -> IO Symbol
newSymbol name = liftM Sy ptr where
    ptr = withCString name ginac_symbol_new 
      >>= newForeignPtr ginac_basic_free_fun

toString :: Expr -> String
toString (Ex ptr) = unsafePerformIO io where
    io = withForeignPtr ptr ginac_ex_to_str >>= peekCString
