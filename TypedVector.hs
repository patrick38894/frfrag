{-# Language DataKinds, GADTs, KindSignatures, MultiParamTypeClasses, StandaloneDeriving #-}
module TypedVector where

data N = N2 
       | N3 
       | N4
       deriving (Eq, Ord, Show)

data Vec (n :: N) a where
    Vec2 :: a -> a -> Vec N2 a
    Vec3 :: a -> a -> a -> Vec N3 a
    Vec4 :: a -> a -> a -> a -> Vec N4 a

deriving instance Show a => Show (Vec n a)
