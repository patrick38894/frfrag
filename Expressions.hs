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

------------------------------------------------------------------------------
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
    App :: Expr (t -> u) -> Expr t -> Expr u
    Last :: Stream (Expr a) -> Expr a

data Stream a where
    Sequence :: Int -> Int -> Int -> Stream (Expr Int)
    Map :: Expr (a -> b) -> Stream (Expr a) -> Stream (Expr b)
    Take :: Expr (x -> Bool) -> Stream (Expr x) -> Stream (Expr x)
    Scan :: Expr (x -> a -> a) -> Expr a -> Stream (Expr x) -> Stream (Expr a)

