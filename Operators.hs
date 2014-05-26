{-# Language FlexibleInstances, StandaloneDeriving, DeriveFunctor, FlexibleContexts #-}
module Operators where
import CoreLanguage
import Primitive
import Utility
import Vector

deriving instance Functor VecN
deriving instance Functor MatN

infixr 0 \$
infixr 1 \>
infixr 1 \^
infixr 2 .&&
infixr 2 .||
infixr 3 .<
infixr 3 .<=
infixr 3 .>
infixr 3 .>=

(\^) :: (Expr t -> Expr u) -> Expr (t -> u)
(\^) = Lift
(\>) :: Expr t -> Expr u -> Expr (t -> u)
--(Val a) \> b = Lam a b
(\>) = undefined
(\$) :: Expr (t -> u) -> Expr t -> Expr u
(\$) = App
(\.) :: Expr (u -> v) -> Expr (t -> u) -> Expr (t -> v)
(\.) = Comp

(.&&) = andE
(.||) = orE
(.<) = ltE
(.<=) = lteE
(.>) = gtE
(.>=) = gteE

unknownDimErr = error "Constructing vector of unknown dimension"

negE :: (Num a, Num (Expr a)) => Expr a -> Expr a
negE a = subE (asTypeOf 0 a) a

instance Num a => Num (VecN a) where
    (+) = zipVec (+)
    (*) = zipVec (*)
    abs = fmap abs
    signum = fmap signum
    negate = fmap negate
    fromInteger = unknownDimErr

instance Num (Expr Int) where
    (+) = addE
    (*) = mulE
    abs = absE
    signum = sgnE
   -- negate = negE
    fromInteger = Int . fromInteger

instance Num (Expr Float) where
    (+) = addE
    (*) = mulE
    abs = absE
    signum = sgnE
    --negate = negE
    fromInteger = Float . fromInteger

instance Num a => Num (Expr (VecN a)) where
    (+) = addE
    (*) = mulE
    abs = absE
    signum = sgnE
    negate = negE
    fromInteger = unknownDimErr

instance Fractional a => Fractional (VecN a) where
    (/) = zipVec (/)
    fromRational = unknownDimErr

instance Fractional (Expr Float) where
    (/) = fdivE
    fromRational = Float . fromRational

instance Fractional a => Fractional (Expr (VecN a)) where
    (/) = fdivE
    fromRational = unknownDimErr

instance Floating a => Floating (VecN a) where
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
    exp = expE
    log = logE
    sin = sinE
    cos = cosE
    asin = asinE
    acos = acosE
    atan = atanE
    sinh = sinhE
    cosh = coshE
    asinh = asinhE
    acosh = acoshE
    atanh = atanhE

instance Floating a =>  Floating (Expr (VecN a)) where
    pi = unknownDimErr
    exp = expE
    log = logE
    sin = sinE
    cos = cosE
    asin = asinE
    acos = acosE
    atan = atanE
    sinh = sinhE
    cosh = coshE
    asinh = asinhE
    acosh = acoshE
    atanh = atanhE
