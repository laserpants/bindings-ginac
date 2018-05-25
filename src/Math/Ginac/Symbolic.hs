module Math.Ginac.Symbolic where

import Math.Ginac

import qualified Math.Ginac as Ex

instance Num Expr where
    x + y        = add x y
    x * y        = mul x y
    negate       = neg
    abs          = Ex.abs
    signum       = Ex.signum
    fromInteger  = num . fromIntegral

instance Fractional Expr where
    x / y        = Ex.div x y
    fromRational = rational

instance Eq Expr where
    x == y       = eql x y

instance Ord Expr where
    compare      = ord
