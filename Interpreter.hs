{-# Language DeriveFunctor #-}

import Language
import Vector
import Swiz

newtype Simp a = Simp {simplify :: a} deriving Functor

instance Expr Simp where
    bool  x = Simp x
    int   x = Simp x
    float x = Simp x
    vec   x = Simp (fmap simplify x)
    mulOp s x y = Simp $ mulOp s (fmap simplify $ Simp x) (fmap simplify $ Simp y)
