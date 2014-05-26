{-# Language GADTs #-}
module Vector where

data N = N2 | N3 | N4 deriving (Eq, Ord)

data VecN a where 
    Vec2 :: a -> a -> VecN a
    Vec3 :: a -> a -> a -> VecN a
    Vec4 :: a -> a -> a -> a -> VecN a

data MatN t = MatN (VecN (VecN t))

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


vecSize v = case v of
    Vec2 _ _ -> N2
    Vec3 _ _ _ -> N3
    Vec4 _ _ _ _-> N4

vX, vY, vZ, vW :: VecN a -> a
vX v = case v of Vec2 a _ -> a; Vec3 a _ _ -> a; Vec4 a _ _ _ -> a
vY v = case v of Vec2 _ b -> b; Vec3 _ b _ -> b; Vec4 _ b _ _ -> b
vZ v = case v of Vec3 _ _ c -> c; Vec4 _ _ c _ -> c
                 other -> error "Vector index too large"
vW v = case v of Vec4 _ _ _ d -> d; other -> error "Vector index too large"


