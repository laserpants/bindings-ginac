module Math.Ginac.FFI.ForeignPtr where

import Foreign
import Math.Ginac.FFI

-- | Opaque pointer for garbage collected GiNaC::ex objects
type GinacExPtr = ForeignPtr GinacEx

-- | Opaque pointer for garbage collected GiNaC::symbol objects
type GinacSymbolPtr = ForeignPtr GinacSymbol
