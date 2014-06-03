{-# Language FlexibleInstances, OverlappingInstances #-}
module Ops where
import Language
import Control.Monad


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

(.@) :: Expr expr => expr (Mat a) -> String -> expr b
(.@) = swiz


(.<), (.>), (.>=), (.<=), (.==), (.&&), (.||) :: Expr expr => expr a -> expr a -> expr Bool
(.<) = compOp "<"
(.>) = compOp ">"
(.<=) = compOp "<="
(.>=) = compOp ">="
(.==) = compOp "=="
(.&&) = compOp "&&"
(.||) = compOp "||"

(.-), (./), (.+), (.*) :: Expr expr => expr a -> expr b -> expr c
(.-) = addOp "-"
(.+) = addOp "+"
(.*) = mulOp "*"
(./) = mulOp "/"

dot :: Expr expr => expr (Mat Float) -> expr (Mat Float) -> expr Float
dot = prim2 "dot"


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
