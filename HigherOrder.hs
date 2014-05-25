{-# Language FlexibleContexts, GADTs, MultiParamTypeClasses #-}
module HigherOrder where
import CoreLanguage

class HFunctor f where
    hfmap :: (Wrap f a, Extract f) => (a -> b) -> f a -> f b
 
class Wrap f a where wrap :: a -> f a
class Extract f where
    extract :: f a -> a

instance Extract Expr where
    extract e = case e of
        Float x -> x
        Int x -> x
        Vec x -> x
        Mat x -> x

instance Wrap Expr Int where wrap = Int
instance Wrap Expr Float where wrap = Float
instance Wrap Expr Bool where wrap = Bool
instance Wrap Expr a => Wrap Expr (VecN a) where wrap = Vec


