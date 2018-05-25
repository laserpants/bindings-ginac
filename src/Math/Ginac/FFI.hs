module Math.Ginac.FFI where

import Foreign
import Foreign.C.String

-- | Data type to represent the GiNaC::ex C++ type
data GinacEx

-- | Data type to represent the GiNaC::symbol C++ type
data GinacSymbol

-- | Data type to represent the GiNaC::add C++ type
data GinacAdd

-- | Data type to represent the GiNaC::mul C++ type
data GinacMul

-- | Data type to represent the GiNaC::power C++ type
data GinacPow

-- | Data type to represent the GiNaC::function C++ type
data GinacFunction

-- | Data type to represent the GiNaC::relational C++ type
data GinacRelational

foreign import ccall "ginac_ex_new"
    ginac_ex_new :: IO (Ptr GinacEx)

foreign import ccall "ginac_ex_new_from_basic"
    ginac_ex_new_from_basic :: Ptr a -> IO (Ptr GinacEx)

foreign import ccall "ginac_ex_new_from_int"
    ginac_ex_new_from_int :: Int -> IO (Ptr GinacEx)

foreign import ccall "ginac_ex_new_from_double"
    ginac_ex_new_from_double :: Int -> IO (Ptr GinacEx)

foreign import ccall "ginac_ex_new_from_relation_eq"
    ginac_ex_new_from_relation_eq :: Ptr GinacEx -> Ptr GinacEx -> IO (Ptr GinacEx)

foreign import ccall "ginac_ex_subs"
    ginac_ex_subs :: Ptr GinacEx -> Ptr GinacRelational -> IO (Ptr GinacEx)

foreign import ccall "ginac_ex_subs_int"
    ginac_ex_subs_int :: Int -> Ptr GinacEx -> Ptr GinacSymbol -> IO (Ptr GinacEx)

foreign import ccall "ginac_ex_free"
    ginac_ex_free :: Ptr GinacEx -> IO ()

foreign import ccall "&ginac_ex_free"
    ginac_ex_free_fun :: FunPtr (Ptr GinacEx -> IO ())

foreign import ccall "ginac_ex_equal"
    ginac_ex_equal :: Ptr GinacEx -> Ptr GinacEx -> IO Bool

foreign import ccall "ginac_ex_compare"
    ginac_ex_compare :: Ptr GinacEx -> Ptr GinacEx -> IO Int

foreign import ccall "ginac_ex_is_numeric"
    ginac_ex_is_numeric :: Ptr GinacEx -> IO Bool

foreign import ccall "ginac_ex_to_double"
    ginac_ex_to_double :: Ptr GinacEx -> IO Double

foreign import ccall "ginac_ex_to_int"
    ginac_ex_to_int :: Ptr GinacEx -> IO Int

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

foreign import ccall "ginac_symbol_static"
    ginac_symbol_static :: IO (Ptr GinacSymbol)

foreign import ccall "ginac_ex_neg"
    ginac_ex_neg :: Ptr GinacEx -> IO (Ptr GinacEx)

foreign import ccall "ginac_ex_abs"
    ginac_ex_abs :: Ptr GinacEx -> IO (Ptr GinacEx)

foreign import ccall "ginac_ex_signum"
    ginac_ex_signum :: Ptr GinacEx -> IO (Ptr GinacEx)

foreign import ccall "ginac_ex_sqrt"
    ginac_ex_sqrt :: Ptr GinacEx -> IO (Ptr GinacEx)

foreign import ccall "ginac_ex_add"
    ginac_ex_add :: Ptr GinacEx -> Ptr GinacEx -> IO (Ptr GinacEx)

foreign import ccall "ginac_ex_mul"
    ginac_ex_mul :: Ptr GinacEx -> Ptr GinacEx -> IO (Ptr GinacEx)

foreign import ccall "ginac_ex_div"
    ginac_ex_div :: Ptr GinacEx -> Ptr GinacEx -> IO (Ptr GinacEx)

foreign import ccall "ginac_ex_diff"
    ginac_ex_diff :: Int -> Ptr GinacEx -> Ptr GinacSymbol -> IO (Ptr GinacEx)

foreign import ccall "ginac_ex_pow"
    ginac_ex_pow :: Ptr GinacEx -> Ptr GinacEx -> IO (Ptr GinacEx)

foreign import ccall "ginac_ex_factorial"
    ginac_ex_factorial :: Int -> IO (Ptr GinacEx)

foreign import ccall "ginac_ex_series"
    ginac_ex_series :: Ptr GinacEx -> Ptr GinacRelational -> Int -> IO (Ptr GinacEx)

foreign import ccall "ginac_ex_coeff"
    ginac_ex_coeff :: Ptr GinacEx -> Ptr GinacEx -> Int -> IO (Ptr GinacEx)

foreign import ccall "ginac_ex_coeff_symbol"
    ginac_ex_coeff_symbol :: Int -> Ptr GinacEx -> Ptr GinacSymbol -> IO (Ptr GinacEx)

foreign import ccall "ginac_add"
    ginac_add :: Ptr GinacEx -> Ptr GinacEx -> IO (Ptr GinacAdd)

foreign import ccall "ginac_mul"
    ginac_mul :: Ptr GinacEx -> Ptr GinacEx -> IO (Ptr GinacMul)

foreign import ccall "ginac_pow"
    ginac_pow :: Ptr GinacEx -> Ptr GinacEx -> IO (Ptr GinacPow)

foreign import ccall "ginac_factorial"
    ginac_factorial :: Ptr GinacEx -> Ptr GinacEx -> IO (Ptr GinacFunction)

foreign import ccall "ginac_relation_eq_new"
    ginac_relation_eq_new :: Ptr GinacEx -> Ptr GinacEx -> IO (Ptr GinacRelational)
