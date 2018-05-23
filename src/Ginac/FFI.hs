module Ginac.FFI where

import Foreign
import Foreign.C.String

-- | Data type to represent the GiNaC::ex C++ type
data GinacEx

-- | Data type to represent the GiNaC::symbol C++ type
data GinacSymbol

foreign import ccall "ginac_ex_new"
    ginac_ex_new :: IO (Ptr GinacEx)

foreign import ccall "ginac_ex_new_from_basic"
    ginac_ex_new_from_basic :: Ptr a -> IO (Ptr GinacEx)

foreign import ccall "ginac_ex_new_from_int"
    ginac_ex_new_from_int :: Int -> IO (Ptr GinacEx)

foreign import ccall "ginac_ex_subs"
    ginac_ex_subs :: Ptr GinacEx -> Ptr GinacSymbol -> Ptr GinacEx -> IO (Ptr GinacEx)

foreign import ccall "ginac_ex_subs_int"
    ginac_ex_subs_int :: Ptr GinacEx -> Ptr GinacSymbol -> Int -> IO (Ptr GinacEx)

foreign import ccall "ginac_ex_free"
    ginac_ex_free :: Ptr GinacEx -> IO ()

foreign import ccall "&ginac_ex_free"
    ginac_ex_free_fun :: FunPtr (Ptr GinacEx -> IO ())

foreign import ccall "ginac_ex_to_str"
    ginac_ex_to_str :: Ptr GinacEx -> IO CString

foreign import ccall "ginac_ex_print"
    ginac_ex_print :: Ptr GinacEx -> IO ()

foreign import ccall "ginac_basic_free"
    ginac_basic_free :: Ptr a -> IO ()

foreign import ccall "&ginac_basic_free"
    ginac_basic_free_fun :: FunPtr (Ptr a -> IO ())

foreign import ccall "ginac_symbol_new"
    ginac_symbol_new :: CString -> IO (Ptr GinacSymbol)

foreign import ccall "ginac_add"
    ginac_add :: Ptr GinacEx -> Ptr GinacEx -> IO (Ptr GinacEx)
