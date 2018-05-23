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

add :: Expr -> Expr -> Expr
add (Ex p) (Ex q) = expr (withForeignPtr p (withForeignPtr q . ginac_add))

mul :: Expr -> Expr -> Expr
mul (Ex p) (Ex q) = expr (withForeignPtr p (withForeignPtr q . ginac_mul))

div :: Expr -> Expr -> Expr
div = undefined

pow :: Expr -> Expr -> Expr
pow = undefined

neg :: Expr -> Expr
neg (Ex ptr) = expr (withForeignPtr ptr ginac_ex_neg)

abs :: Expr -> Expr
abs = undefined

signum :: Expr -> Expr
signum = undefined

diff :: Expr -> Expr
diff = undefined

diffn :: Int -> Expr -> Expr
diffn nth = undefined

factorial :: Int -> Expr
factorial = undefined

sqrt :: Expr -> Expr
sqrt = undefined

rational :: Rational -> Expr
rational = undefined

newSymbol :: String -> IO Symbol
newSymbol name = liftM Sy ptr where
    ptr = withCString name ginac_symbol_new 
      >>= newForeignPtr ginac_basic_free_fun

toString :: Expr -> String
toString (Ex ptr) = unsafePerformIO io where
    io = withForeignPtr ptr ginac_ex_to_str >>= peekCString
