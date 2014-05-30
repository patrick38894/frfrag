module Num where
import Language
import Vector

instance Num a => Num (VecN a) where
    (+) = zipVec (+)
    (*) = zipVec (*)
    abs = mapVec abs
    signum = mapVec signum
    fromInteger i = vecFromList (replicate 4 (fromInteger i))

instance Fractional a => Fractional (VecN a) where
    fromRational r = vecFromList (replicate 4 (fromRational r))

instance Floating a => Floating (VecN a)

instance Num Expr where
    (+) = BinOp "+" 
    (*) = BinOp "*" 
    abs = Prim "abs"
    signum = Prim "sign"
    fromInteger = Int . fromInteger 

instance Fractional Expr where
    fromRational = Float . fromRational
    (/) = BinOp "/"

instance Floating Expr where
    pi = Float pi
    sin = Prim "sin" 
    cos = Prim "cos" 
    tan = Prim "tan" 
    exp = Prim "exp" 
    log = Prim "log" 
    asin = Prim "asin" 
    acos = Prim "acos" 
    atan = Prim "atan" 
    sinh = Prim "sinh" 
    cosh = Prim "cosh" 
    asinh = Prim "asinh" 
    acosh = Prim "acosh" 
    atanh = Prim "atanh" 
