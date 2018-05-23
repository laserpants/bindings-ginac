module Main where

import Foreign
import Foreign.C.String
import Foreign.Marshal.Alloc
import Ginac
import Ginac.FFI
import Ginac.FFI.ForeignPtr
import Ginac.Symbolic

main :: IO ()
main = do

    sx <- newSymbol "x"
    let a = add (var sx) (num 5)
    print (toString a)

    let x = var sx

    let b = 1 / (1 - x)
    print (toString b)


--    x <- withCString "x" ginac_symbol_new
--    p <- ginac_ex_new_from_basic x
--    q <- ginac_ex_new_from_int 1
--    r <- ginac_add_new p q
--    s <- ginac_ex_new_from_basic r
--
--    t <- ginac_ex_to_str s
--    peekCString t >>= print
--    free t
--
--    v <- ginac_ex_subs_int s x 5
--
--    w <- ginac_ex_to_str v
--    peekCString w >>= print
--    free w
--
--    ginac_ex_free v
--    ginac_ex_free s
--    ginac_basic_free r
--    ginac_ex_free q
--    ginac_ex_free p
--    ginac_basic_free x
--
--    pure ()
