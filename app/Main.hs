module Main where

import Foreign.C.String
import Ginac.FFI

main :: IO ()
main = do

    x <- withCString "x" ginac_symbol_new
    p <- ginac_ex_new_from_basic x

    ginac_ex_print p

    ginac_ex_free p
    ginac_basic_free x

    pure ()
