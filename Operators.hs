{-# Language 
            FlexibleInstances, 
            StandaloneDeriving, 
            DeriveFunctor, 
            FlexibleContexts,
            UndecidableInstances #-}
module Operators where
import Expressions
import Primitive
import Utility
import Vector
import HigherOrder

infixr 2 .&&
infixr 2 .||
infixr 3 .<
infixr 3 .<=
infixr 3 .>
infixr 3 .>=
(.&&) = andE
(.||) = orE
(.<) = ltE
(.<=) = lteE
(.>) = gtE
(.>=) = gteE

unknownDimErr = error "Constructing vector of unknown dimension"

negE :: (Eq a, Num a, Num (Expr a), Pretty a, Wrap Expr a, Wrap Rep a) => Expr (a -> a)
negE = Rewrite freshSym (App (App subE (asTypeOf 0 freshSym)) freshSym)

instance (Num a, Wrap Rep a) => Num (VecN a) where
    (+) = zipVec (+)
    (*) = zipVec (*)
    abs = fmap abs
    signum = fmap signum
    negate = fmap negate
    fromInteger = unknownDimErr

instance Num (Expr Int) where
    (+) = App . App addE
    (*) = App . App mulE
    abs = App absE
    signum = App sgnE
    negate = App negE
    fromInteger = Int . fromInteger

instance Num (Expr Float) where
    (+) = App . App addE
    (*) = App . App mulE
    abs = App absE
    signum = App sgnE
    negate = App negE
    fromInteger = Float . fromInteger

instance (Eq a, Num a, Pretty a, Wrap Expr a, Wrap Rep a) => Num (Expr (VecN a)) where
    (+) = App . App addE
    (*) = App . App mulE
    abs = App absE
    signum = App sgnE
    negate = App negE
    fromInteger = unknownDimErr

instance (Fractional a, Wrap Rep a) => Fractional (VecN a) where
    (/) = zipVec (/)
    fromRational = unknownDimErr

instance Fractional (Expr Float) where
    (/) = App . App fdivE
    fromRational = Float . fromRational

instance (Eq a, Fractional a, Pretty a, Wrap Expr a, Wrap Rep a) => Fractional (Expr (VecN a)) where
    (/) = App . App fdivE
    fromRational = unknownDimErr

instance (Floating a, Wrap Rep a) => Floating (VecN a) where
    pi = unknownDimErr
    exp = fmap exp
    log = fmap log
    sin = fmap sin
    cos = fmap cos
    asin = fmap asin
    acos = fmap acos
    atan = fmap atan
    sinh = fmap sinh
    cosh = fmap cosh
    asinh = fmap asinh
    acosh = fmap acosh
    atanh = fmap atanh

instance Floating (Expr Float) where
    pi = Float pi
    exp = App expE
    log = App logE
    sin = App sinE
    cos = App cosE
    asin = App asinE
    acos = App acosE
    atan = App atanE
    sinh = App sinhE
    cosh = App coshE
    asinh = App asinhE
    acosh = App acoshE
    atanh = App atanhE

instance (Eq a, Floating a, Pretty a, Wrap Expr a, Wrap Rep a) =>  Floating (Expr (VecN a)) where
    pi = unknownDimErr
    exp = App expE
    log = App logE
    sin = App sinE
    cos = App cosE
    asin = App asinE
    acos = App acosE
    atan = App atanE
    sinh = App sinhE
    cosh = App coshE
    asinh = App asinhE
    acosh = App acoshE
    atanh = App atanhE
