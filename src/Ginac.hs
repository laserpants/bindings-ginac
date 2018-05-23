module Ginac 
  ( Expr
  , Symbol
  , add
  , num
  , symbol
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

symbol :: String -> IO Symbol
symbol name = liftM Sy ptr where
    ptr = withCString name ginac_symbol_new 
      >>= newForeignPtr ginac_basic_free_fun

toString :: Expr -> String
toString (Ex ptr) = unsafePerformIO io where
    io = withForeignPtr ptr ginac_ex_to_str >>= peekCString
