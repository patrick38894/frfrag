{-# Language FlexibleInstances, OverlappingInstances #-}
module Ops where
import Language
import Control.Monad

-- Fixity mirroring that of real GLSL
infixr 7 .@
infixl 6 .*
infixl 6 ./
infixl 5 .+
infixl 5 .-
infixr 4 .<
infixr 4 .>
infixr 4 .<=
infixr 4 .>=
infixr 3 .==
infixr 2 .&&
infixr 1 .||

-- Infix operator for swizzle syntax
(.@) :: Expr expr => expr (Mat a) -> String -> expr b
(.@) = swiz

-- Comparison operators
(.<), (.>), (.>=), (.<=), (.==), (.&&), (.||) :: Expr expr => expr a -> expr a -> expr Bool
(.<) = compOp "<"
(.>) = compOp ">"
(.<=) = compOp "<="
(.>=) = compOp ">="
(.==) = compOp "=="
(.&&) = compOp "&&"
(.||) = compOp "||"

-- Generic numeric operators
-- These work on differently typed inputs,
-- e.g. a matrix and a scalar,
-- so are a bit more general than the num instances.
(.-), (./), (.+), (.*) :: Expr expr => expr a -> expr b -> expr c
(.-) = addOp "-"
(.+) = addOp "+"
(.*) = mulOp "*"
(./) = mulOp "/"

-- Dot product for two vectors
dot :: Expr expr => expr (Mat Float) -> expr (Mat Float) -> expr Float
dot = prim2 "dot"

-- Num, fractional, rational instances for matrixes and expressions
instance (Num a, Show a, Tag a) => Num (TagE (Mat a)) where
    (+) = (.+)
    (-) = (.-)
    (*) = (.*)
    abs = prim "abs"
    signum = prim "sign"
    fromInteger = matrix . matriculate . fromInteger 

instance Fractional (TagE (Mat Float)) where
    fromRational = lit . matriculate . fromRational
    (/) = (./)

instance Floating (TagE (Mat Float)) where
    pi = lit $ matriculate pi
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

instance (Num a, Tag a, Expr repr, Show a) => Num (repr a) where
    (+) = (.+)
    (-) = (.-)
    (*) = (.*)
    abs = prim "abs"
    signum = prim "sign"
    fromInteger = lit . fromInteger 

instance Expr repr => Fractional (repr Float) where
    fromRational = lit . fromRational
    (/) = (./)

instance Expr repr => Floating (repr Float) where
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

matriculate x = Mat [[x]]
