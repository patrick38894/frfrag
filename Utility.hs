{-# Language FlexibleContexts, FlexibleInstances, GADTs #-}
module Utility where
import Expressions
import HigherOrder
import Vector
import Synonyms

float = FloaT
int = IntT
bool = BoolT

true = Bool True
false = Bool False
freshSym :: Wrap Rep a => Expr a 
freshSym = Sym (PolyT 0) 0

scalar :: Float -> Expr Float
scalar = wrap
vecRepeat :: Int -> Float -> Expr (VecN Float)
vecRepeat n x = vec $ replicate n x

vec2 = vecRepeat 2
vec3 = vecRepeat 3
vec4 = vecRepeat 4

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
    Vec (VecT FloaT N2) (Vec2 a b) -> Vec (VecT FloaT N4) (Vec4 a b (Float 1) (Float 1))
    Val b -> Vec (VecT FloaT N4) (Vec4 (Val $ Swiz b "x") (Val $ Swiz b "y") (Float 1) (Float 1))
    other -> error $ show other
    
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
