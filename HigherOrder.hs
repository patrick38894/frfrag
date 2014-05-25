{-# Language FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses #-}
module HigherOrder where
import CoreLanguage
import Vector

class Wrap f a where wrap :: a -> f a
class Extract f where extract :: f a -> a
class HFunctor f where 
    hfmap :: (Wrap f b, Extract f) => (a -> b) -> f a -> f b
    hfmap f = wrap . f . extract
 
data HFree f where
    Pure :: a -> HFree f
    Roll :: HFunctor f => f (HFree f) -> HFree f

instance Wrap Rep Int where wrap = const IntT
instance Wrap Rep Float where wrap = const FloaT
instance Wrap Rep Bool where wrap = const BoolT
instance Wrap Rep a => Wrap Rep (VecN a) where
    wrap v = VecT (wrap $ vX v) (vecSize v)

instance Extract Decl where 
  extract e = case e of
    Value name expr -> undefined
    Uniform bind def -> undefined
    Procedure proc stmt -> undefined
    Function func expr -> undefined

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

instance HFunctor Expr

count = Sequence 1 1 1
