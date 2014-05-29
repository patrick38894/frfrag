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
    x + y = binOp "+" [x,y]
    x * y = binOp "*" [x,y]
    abs x = prim "abs" [x]
    signum x = prim "sign" [x]
    fromInteger i = Int (fromInteger i) 

instance Fractional Expr where
    fromRational r = Float (fromRational r)

instance Floating Expr where
    pi = Float pi
    sin x = prim "sin" [x]
    cos x = prim "cos" [x]
    tan x = prim "tan" [x]
    exp x = prim "exp" [x]
    log x = prim "log" [x]
    asin x = prim "asin" [x]
    acos x = prim "acos" [x]
    atan x = prim "atan" [x]
    sinh x = prim "sinh" [x]
    cosh x = prim "cosh" [x]
    asinh x = prim "asinh" [x]
    acosh x = prim "acosh" [x]
    atanh x = prim "atanh" [x]
