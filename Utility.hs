{-# Language FlexibleContexts, FlexibleInstances, GADTs #-}
module Utility where
import CoreLanguage
import HigherOrder
import Vector

fragCoord = Val FragCoord
coordX = sX FragCoord
coordY = sY FragCoord

rep2, rep3, rep4 :: a -> VecN a
rep2 y = Vec2 y y
rep3 y = Vec3 y y y
rep4 y = Vec4 y y y y

pointXY :: Num a => a -> a -> VecN a
pointXY a b = Vec4 a b 1 1

rep2E, rep3E, rep4E :: Expr a -> Expr (VecN a)
rep2E = Vec . rep2 . extract
rep3E = Vec . rep3 . extract
rep4E = Vec . rep4 . extract

pointXYE :: Num a => Expr a -> Expr a -> Expr (VecN a)
pointXYE a b = Vec $ pointXY (extract a) (extract b)

gX, gY, gZ, gW :: Wrap Expr a => Expr (VecN a) -> Expr a
gX (Vec v) = wrap (vX v)
gY (Vec v) = wrap (vY v)
gZ (Vec v) = wrap (vZ v)
gW (Vec v) = wrap (vW v)

-- Swizzle a named vector
sX c = Val (Swiz c "x")
sY c = Val (Swiz c "y")
sZ c = Val (Swiz c "z")
sW c = Val (Swiz c "w")

-- Small values as Expr Floats
zero = Float 0
one = Float 1
two = Float 2

