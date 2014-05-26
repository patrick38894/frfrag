{-# Language 
             FlexibleInstances,
             GADTs,
             KindSignatures,
             TypeSynonymInstances,
             UndecidableInstances
 #-}

module CoreLanguage where
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

data Binding :: * -> * where
    FragCoord :: Binding (VecN Float)
    FragColor :: Binding (VecN Float)
    Var :: Rep t -> String -> Binding t
    Swiz :: Binding v -> String -> Binding x
    Func :: Binding r -> Binding a -> Binding (a -> r)

data Decl :: * -> * where
    Value :: Binding t -> Expr t -> Decl t
    Uniform :: Binding t -> Maybe (Expr t) -> Decl t
    Procedure :: String -> Rep r -> Stmt -> Decl (a -> r)
    Function :: String -> Rep r -> Expr (a -> r) -> Decl (a -> r)

data Stream a
    
data Expr :: * -> * where
--  Core GLSL expressions
--  Literals
    Float :: Float -> Expr Float
    Bool :: Bool -> Expr Bool
    Int :: Int -> Expr Int
    Vec :: VecN t -> Expr (VecN t)
    Mat :: MatN t -> Expr (MatN t)
--  Primitive unary and binary functions and ops
    Prim :: String -> Expr a -> Expr b
    Prim2 :: String -> Expr a -> Expr b -> Expr c
    BinOp :: String -> Expr a -> Expr b -> Expr c 
--  GLSL Abstractions
    Val :: Binding t -> Expr t
    Call :: Binding (t -> u) -> Expr (t -> u)
--  Fake expressions (cannot be printed directly). 
--  Term-rewriting abstractions
    Lam :: Int -> Rep t -> Expr u -> Expr (t -> u)
    Lift :: (Expr t -> Expr u) -> Expr (t -> u)
    App :: Expr (t -> u) -> Expr t -> Expr u
    Comp :: Expr (u -> v) -> Expr (t -> u) -> Expr (t -> v)
    Recurse :: Expr (a -> a) -> (Expr a -> Bool) -> Expr a
--  Vector abstractions
    MapV :: Expr (a -> b) -> Expr (VecN a) -> Expr (VecN b)
    AppV :: Expr (VecN (a -> b)) -> Expr (VecN a) -> Expr (VecN b)
    ZipV :: Expr (a -> b -> c) -> Expr (VecN a) -> Expr (VecN b) -> Expr (VecN c)
--  Loop abstractions
    Sequence :: Int -> Int -> Int -> Expr (Stream Int)
    Fold :: Expr (x -> a -> a) -> Expr a -> Expr (Stream x -> a)
    Map :: Expr (a -> b) -> Expr (Stream a -> Stream b)
    Repeat :: Expr (a -> a) -> Expr (a -> Bool) -> Expr (Stream a -> a)

data Stmt where
    Block :: [Stmt] -> Stmt
    DecVar :: Binding t -> Expr t -> Stmt
    Mutate :: Binding t -> Expr t -> Stmt
    Switch :: Expr Int -> [(Int, Stmt)] -> Stmt
    IfElse :: Expr Bool -> Stmt -> Stmt -> Stmt
    For :: Expr Int -> Expr (Int -> Bool) -> Expr (Int -> Int) -> Stmt -> Stmt
    While :: Expr Bool -> Stmt -> Stmt
    Break :: Stmt
    Continue :: Stmt
    Return :: Expr a -> Stmt
    Terminate :: Stmt
    Discard :: Stmt
