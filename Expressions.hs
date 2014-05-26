{-# Language 
             FlexibleInstances,
             GADTs,
             KindSignatures,
             TypeSynonymInstances,
             UndecidableInstances
 #-}

module Expressions where
import Vector
import Text.PrettyPrint.HughesPJ hiding (float, int)


------------------------------------------Var ------------------------------------
-- GLSL language representation ----------------------------------------------
data Rep :: * -> * where
    BoolT :: Rep Bool
    IntT :: Rep Int
    FloaT :: Rep Float
    FuncT :: Rep r -> Rep a -> Rep (a -> r)
    VecT :: Rep t -> N -> Rep (VecN t)
    MatT :: Rep t -> N -> N -> Rep (MatN t)
    PolyT :: Rep a
    VoidT :: Rep ()

data Binding :: * -> * where
    FragCoord :: Binding (VecN Float)
    FragColor :: Binding (VecN Float)
    Var :: Rep t -> String -> Binding t
    Swiz :: Binding v -> String -> Binding x
    Func :: Binding r -> Binding a -> Binding (a -> r)
    Void :: Binding ()

data Stream a
    
data Expr :: * -> * where
--  Core GLSL expressions
--  Literals
    Float :: Float -> Expr Float
    Bool :: Bool -> Expr Bool
    Int :: Int -> Expr Int
    Vec :: Rep (VecN t) -> VecN t -> Expr (VecN t)
    Mat :: Rep (MatN t) -> MatN t -> Expr (MatN t)
--  Primitive unary and binary functions and ops
    Prim :: String -> Expr (a -> b)
    Prim2 :: String -> Expr (a -> b -> c)
    BinOp :: String -> Expr (a -> b -> c)
--  GLSL Abstractions
    Val :: Binding t -> Expr t
    Call :: Binding (t -> u) -> Expr (t -> u)
--  Fake expressions (cannot be printed directly). 
--  Term-rewriting abstractions
    Lam :: Int -> Rep t -> Expr u -> Expr (t -> u)
    LamT :: Int -> Expr a
    Lift :: (Expr t -> Expr u) -> Expr (t -> u)
    App :: Expr (t -> u) -> Expr t -> Expr u
    Comp :: Expr (u -> v) -> Expr (t -> u) -> Expr (t -> v)
    Recurse :: Expr (a -> a) -> (Expr a -> Bool) -> Expr a
--  Vector abstractions
    MapV :: Expr (a -> b) -> Expr (VecN a -> VecN b)
    AppV :: Expr (VecN (a -> b)) -> Expr (VecN a -> VecN b)
    ZipV :: Expr (a -> b -> c) -> Expr (VecN a -> VecN b -> VecN c)
--  Loop abstractions
    Sequence :: Int -> Int -> Int -> Expr (Stream Int)
    Fold :: Expr (x -> a -> a) -> Expr a -> Expr (Stream x -> a)
    Map :: Expr (a -> b) -> Expr (Stream a -> Stream b)
    Repeat :: Expr (a -> a) -> Expr (a -> Bool) -> Expr (Stream a -> a)


