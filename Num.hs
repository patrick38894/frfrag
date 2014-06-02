{-# Language FlexibleInstances, OverlappingInstances #-}
module Num where
import Language
import Control.Monad

instance Num a => Num (Mat a)

instance (Num a, Tag a, Expr repr, Show a) => Num (repr a) where
    (+) = addOp "+"
    (-) = addOp "-"
    (*) = mulOp "*"
    abs = prim "abs"
    signum = prim "sign"
    fromInteger = lit . fromInteger 

instance (Fractional a, Tag a, Expr repr, Show a) => Fractional (repr a) where
    fromRational = lit . fromRational
    (/) = mulOp "/"

instance (Floating a, Tag a, Expr repr, Show a) => Floating (repr a) where
    pi = lit pi
    sin = prim "sin" 
    cos = prim "cos" 
    tan = prim "tan" 
    exp = prim "exp" 
    log = prim "log" 
    asin = prim "asin" 
    acos = prim "acos" 
    atan = prim "atan" 
    sinh = prim "sinh" 
    cosh = prim "cosh" 
    asinh = prim "asinh" 
    acosh = prim "acosh" 
    atanh = prim "atanh"
