module Main where

import Foreign
import Foreign.C.String
import Foreign.Marshal.Alloc
import Math.Ginac
import Math.Ginac.FFI
import Math.Ginac.FFI.ForeignPtr
import Math.Ginac.Symbolic

main :: IO ()
main = do

    s_x <- newSymbol "x"

    let a = add (var s_x) (num 5)
    print (toString a)

    let x = var s_x

    let b = 1 / (1 - x)
    print (toString b)

    let c = subsInt (diffn 5 b s_x) s_x 0
    print (toString c)

    let c' = subsInt (diffn 4 b s_x) s_x 0
    print (toString c')

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
