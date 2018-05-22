module Ginac.FFI where

import Foreign
import Foreign.C.String

-- | Data type to represent the GiNaC::ex C++ type
data GinacEx

-- | Data type to represent the GiNaC::symbol C++ type
data GinacSymbol

-- | Opaque pointer for garbage collected GiNaC::ex objects
type GinacExPtr = ForeignPtr GinacEx

-- | Opaque pointer for garbage collected GiNaC::symbol objects
type GinacSymbolPtr = ForeignPtr GinacSymbol

foreign import ccall "ginac_ex_new"
    ginac_ex_new :: IO (Ptr GinacEx)

foreign import ccall "ginac_ex_new_from_int"
    ginac_ex_new_from_int :: Int -> IO (Ptr GinacEx)

foreign import ccall "ginac_ex_new_from_symbol"
    ginac_ex_new_from_symbol :: Ptr GinacSymbol -> IO (Ptr GinacEx)

foreign import ccall "ginac_ex_free"
    ginac_ex_free :: Ptr GinacEx -> IO ()

foreign import ccall "&ginac_ex_free"
    ginac_ex_free_fun :: FunPtr (Ptr GinacEx -> IO ())

foreign import ccall "ginac_ex_to_str"
    ginac_ex_to_str :: Ptr GinacEx -> IO CString

foreign import ccall "ginac_ex_print"
    ginac_ex_print :: Ptr GinacEx -> IO ()