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

infixr 0 \$
infixr 1 \>
infixr 1 \^

(\>) :: Expr t -> Expr u -> Expr (t -> u)
--(Val a) \> b = Lam a b
(\>) = undefined
(\$) :: Expr (t -> u) -> Expr t -> Expr u
(\$) = App
(\.), composeE :: Expr (u -> v) -> Expr (t -> u) -> Expr (t -> v)
(\.) = composeE
composeE = undefined
(\^), liftE :: (Expr t -> Expr u) -> Expr (t -> u)
(\^) = liftE
liftE = undefined

curryE :: (Expr a -> Expr b -> Expr c) -> Expr (a -> b -> c)
curryE = undefined

uncurryE :: Expr (a -> b -> c) -> Expr a -> Expr b -> Expr c
uncurryE = undefined

staticRecursion :: (Expr a -> a) -> Expr (a -> a) -> Expr a
staticRecursion base f = undefined

mapV :: Expr (a -> b) -> Expr (VecN a -> VecN b)
mapV = undefined
appV :: Expr (VecN (a -> b)) -> Expr (VecN a -> VecN b)
appV fs = liftE $ (uncurryE . zipV $ curryE App) fs
zipV :: Expr (a -> b -> c) -> Expr (VecN a -> VecN b -> VecN c)
zipV = undefined

sequenceS = Sequence
mapS = Map
takeS = Take
lastS = Last
scanS = Scan

foldS :: Expr (x -> a -> a) -> Expr a -> Stream (Expr x) -> Expr a
foldS f a = lastS . scanS f a

accumWhileS :: Expr (a -> Bool) -> Expr (x -> a -> a) -> Expr a -> Stream (Expr x) -> Expr a
accumWhileS c f a = lastS . takeS c . scanS f a
