{-# Language FlexibleContexts, 
             FlexibleInstances,
             GADTs, 
             MultiParamTypeClasses,
             UndecidableInstances#-}

module HigherOrder where
import Expressions
import Vector

class Wrap f a where wrap :: a -> f a
class Extract f where extract :: f a -> a

class HFunctor f where 
    hfmap :: Wrap f b => (a -> b) -> f a -> f b

instance (Extract f) => HFunctor f where
    hfmap f = wrap . f . extract
 
instance Wrap Rep Int where wrap = const IntT
instance Wrap Rep Float where wrap = const FloaT
instance Wrap Rep Bool where wrap = const BoolT
instance Wrap Rep a => Wrap Rep (VecN a) where
    wrap v = VecT (wrap $ vX v) (vecSize v)

instance Extract Expr where 
  extract e = case e of
    Float x -> x
    Int x -> x
    Vec r x -> x
    Mat r x -> x

instance Wrap Expr Int where wrap = Int
instance Wrap Expr Float where wrap = Float
instance Wrap Expr Bool where wrap = Bool
instance (Wrap Rep a, Wrap Expr a) => Wrap Expr (VecN a) where
    wrap v = Vec (case v of Vec2 a _ -> VecT (wrap a) N2
                            Vec3 a _ _ -> VecT (wrap a) N3
                            Vec4 a _ _ _ -> VecT (wrap a) N4) v
instance Wrap Rep (a -> b) where wrap = error "No GLSL representation for function types"
instance Wrap Expr (a -> b) where wrap = error "No GLSL representation for function types"

count = Sequence 1 1 1
