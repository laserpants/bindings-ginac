module Main where

import Ginac.FFI

main :: IO ()
main = do
    p <- ginac_ex_new_from_int 5
    ginac_ex_print p
    ginac_ex_free p
    pure ()
