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

instance Floating a => Floating (VecN a) where
    sin = mapVec sin
    cos = mapVec cos
    tan = mapVec tan
    exp = mapVec exp 
    log = mapVec log 
    asin = mapVec asin 
    acos = mapVec acos 
    atan = mapVec atan 
    sinh = mapVec sinh 
    cosh = mapVec cosh 
    asinh = mapVec asinh 
    acosh = mapVec acosh 
    atanh = mapVec atanh
 
instance Num Expr where
    (+) = BinOp PolyT PolyT PolyT "+" 
    (-) = BinOp PolyT PolyT PolyT "-"
    (*) = BinOp PolyT PolyT PolyT "*" 
    abs = Prim PolyT PolyT "abs"
    signum = Prim PolyT PolyT "sign"
    fromInteger = Int . fromInteger 

instance Fractional Expr where
    fromRational = Float . fromRational
    (/) = BinOp PolyT PolyT PolyT "/"

instance Floating Expr where
    pi = Float pi
    sin = Prim PolyT PolyT "sin" 
    cos = Prim PolyT PolyT "cos" 
    tan = Prim PolyT PolyT "tan" 
    exp = Prim PolyT PolyT "exp" 
    log = Prim PolyT PolyT "log" 
    asin = Prim PolyT PolyT "asin" 
    acos = Prim PolyT PolyT "acos" 
    atan = Prim PolyT PolyT "atan" 
    sinh = Prim PolyT PolyT "sinh" 
    cosh = Prim PolyT PolyT "cosh" 
    asinh = Prim PolyT PolyT "asinh" 
    acosh = Prim PolyT PolyT "acosh" 
    atanh = Prim PolyT PolyT "atanh" 
