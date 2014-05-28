{-# Language FlexibleContexts, FlexibleInstances, GADTs #-}
module Utility where
import Expressions
import HigherOrder
import Vector
import Synonyms

fragCoord = Val FragCoord

coordX,coordY :: Wrap Rep Float => Expr Float
coordX = sX FragCoord
coordY = sY FragCoord

rep2, rep3, rep4 :: a -> VecN a
rep2 y = Vec2 y y
rep3 y = Vec3 y y y
rep4 y = Vec4 y y y y

pointXY :: Num a => a -> a -> VecN a
pointXY a b = Vec4 a b 1 1

pointXYE :: Expr (VecN Float) -> Expr (VecN Float)
pointXYE v = case v of 
    Vec vec2 (Vec2 a b) -> Vec vec4 (Vec4 a b 1 1)
    Val b -> Vec vec4 (Vec4 (Swiz b "x") (Swiz b "y") 1 1)
    other -> error $ show other
    
gX, gY, gZ, gW :: Wrap Expr a => Expr (VecN a) -> Expr a
gX (Vec r v) = wrap (vX v)
gY (Vec r v) = wrap (vY v)
gZ (Vec r v) = wrap (vZ v)
gW (Vec r v) = wrap (vW v)

-- Swizzle a named vector
sX, sY, sZ, sW :: Wrap Rep v => Binding (VecN v) -> Expr v
sX c = Val (Swiz c "x")
sY c = Val (Swiz c "y")
sZ c = Val (Swiz c "z")
sW c = Val (Swiz c "w")

-- Small values as Expr Floats
zero = Float 0
one = Float 1
two = Float 2

vec :: [Float] -> Expr (VecN Float)
vec = wrap . vecFromList
