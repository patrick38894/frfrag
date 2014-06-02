{-# Language
        DeriveFunctor,
        PolyKinds #-}

module Vector where

data V2 a = Vec2 a a        deriving (Eq, Functor, Show)
data V3 a = Vec3 a a a      deriving (Eq, Functor, Show)
data V4 a = Vec4 a a a a    deriving (Eq, Functor, Show)

class Functor v => Vec v where
    dim :: v a -> Int
    toList :: v a -> [a]

class Vec2p v
class Vec3p v
class Vec4p v

instance Vec V2 where
    dim = const 2
instance Vec V3 where
    dim = const 3
instance Vec V4 where
    dim = const 4

instance Vec2p V2
instance Vec2p V3
instance Vec2p V4
instance Vec3p V3
instance Vec3p V4
instance Vec4p V4

zipVec :: Vec v => (a -> b -> c) -> v a -> v b -> v c
zipVec = undefined

getLength :: Vec v => v a -> Int
getLength = undefined


getInitial :: Vec v => v a -> String
getInitial = undefined
