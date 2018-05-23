module Main where

import Foreign.C.String
import Foreign.Marshal.Alloc
import Ginac.FFI

main :: IO ()
main = do

    x <- withCString "x" ginac_symbol_new
    p <- ginac_ex_new_from_basic x
    q <- ginac_ex_new_from_int 1
    r <- ginac_add_new p q
    s <- ginac_ex_new_from_basic r

    t <- ginac_ex_to_str s
    u <- peekCString t
    print u
    free t

    ginac_ex_free s
    ginac_basic_free r
    ginac_ex_free p
    ginac_basic_free x

    pure ()
