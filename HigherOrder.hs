{-# Language FlexibleContexts, 
             FlexibleInstances,
             GADTs, 
             MultiParamTypeClasses,
             UndecidableInstances#-}

module HigherOrder where
import Expressions
import Vector

infixr 0 \$
infixr 1 \>
infixr 1 \^

class HFunctor f where 
    hfmap :: Wrap f b => (a -> b) -> f a -> f b

instance (Extract f) => HFunctor f where
    hfmap f = wrap . f . extract
 
(\>) :: Expr t -> Expr u -> Expr (t -> u)
--(Val a) \> b = Lam a b
(\>) = undefined
(\$) :: (Pretty t, Wrap Expr t, Wrap Rep t) =>  Expr (t -> u) -> Expr t -> Expr u
(\$) = App
(\.), composeE :: Expr (u -> v) -> Expr (t -> u) -> Expr (t -> v)
(\.) = composeE
composeE = undefined
(\^), liftE :: (Expr t -> Expr u) -> Expr (t -> u)
(\^) = liftE
liftE = undefined

liftE' :: (t -> u) -> Expr (t -> u)
liftE' f = undefined

curryE :: (Expr a -> Expr b -> Expr c) -> Expr (a -> b -> c)
curryE = undefined

uncurryE :: Expr (a -> b -> c) -> Expr a -> Expr b -> Expr c
uncurryE = undefined

staticRecursion :: (Expr a -> a) -> Expr (a -> a) -> Expr a
staticRecursion base f = undefined

mapV :: Expr (a -> b) -> Expr (VecN a -> VecN b)
mapV = undefined
appV :: (Pretty a, Wrap Expr a, Wrap Rep a) => Expr (VecN (a -> b)) -> Expr (VecN a -> VecN b)
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
