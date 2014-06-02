module Erasure where

data Type = Int
          | Float
          | Bool
          | VecT Type Int
          | Void
          | TyVar Int
          | Error String

class Tag a where tag :: a -> Type

instance Tag Int where tag = const Int
instance Tag Float where tag = const Float
instance Tag Bool where tag = const Bool
instance (Tag a, Vec v) => Tag (v a)
    where tag v = VecT (tag (head $ toList v)) (getLength v)
