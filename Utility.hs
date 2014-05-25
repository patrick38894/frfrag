{-# Language FlexibleInstances, GADTs #-}
module Utility where
import CoreLanguage

extract :: Expr a -> a
extract e = case e of
    Float x -> x
    Int x -> x
    Vec x -> x
    Mat x -> x

class Wrap a where wrap :: a -> Expr a
instance Wrap Int where wrap = Int
instance Wrap Float where wrap = Float
instance Wrap Bool where wrap = Bool
instance Wrap a => Wrap (VecN a) where wrap = Vec

asInt n = (case n of N2 -> 2; N3 -> 3; N4 -> 4) :: Int

vecToList :: VecN t -> [t]
vecToList v = case v of
    Vec2 a b -> [a, b]
    Vec3 a b c -> [a, b, c]
    Vec4 a b c d -> [a, b, c, d]

vecFromList :: [t] -> VecN t
vecFromList l = case l of
    [a,b] -> Vec2 a b
    [a,b,c] -> Vec3 a b c
    [a,b,c,d] -> Vec4 a b c d

zipVec :: (a -> b -> c) -> VecN a -> VecN b -> VecN c
zipVec op a b = vecFromList $ zipWith op (vecToList a) (vecToList b)

vec2  = VecT FloaT N2 
vec3  = VecT FloaT N3 
vec4  = VecT FloaT N4 
ivec2 = VecT IntT  N2 
ivec3 = VecT IntT  N3 
ivec4 = VecT IntT  N4 
bvec2 = VecT BoolT N2 
bvec3 = VecT BoolT N3 
bvec4 = VecT BoolT N4 

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

vX, vY, vZ, vW :: VecN a -> a
vX v = case v of Vec2 a _ -> a; Vec3 a _ _ -> a; Vec4 a _ _ _ -> a
vY v = case v of Vec2 _ b -> b; Vec3 _ b _ -> b; Vec4 _ b _ _ -> b
vZ v = case v of Vec3 _ _ c -> c; Vec4 _ _ c _ -> c
                 other -> error "Vector index too large"
vW v = case v of Vec4 _ _ _ d -> d; other -> error "Vector index too large"

gX, gY, gZ, gW :: Wrap a => Expr (VecN a) -> Expr a
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


