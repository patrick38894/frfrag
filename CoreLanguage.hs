{-# Language FlexibleInstances,
             GADTs,
             PolyKinds,
             UndecidableInstances#-}

module CoreLanguage where
import Control.Monad(guard)
import Data.Maybe(isJust)
import Text.PrettyPrint.HughesPJ

------------------------------------------------------------------------------
-- GLSL language representation ----------------------------------------------
data N = N2 | N3 | N4 deriving (Eq, Ord)

data VecN a where 
    Vec2 :: a -> a -> VecN a
    Vec3 :: a -> a -> a -> VecN a
    Vec4 :: a -> a -> a -> a -> VecN a

data MatN t = MatN (VecN (VecN t))

data Rep :: * -> * where
    BoolT :: Rep Bool
    IntT :: Rep Int
    FloaT :: Rep Float
    VecT :: Rep t -> N -> Rep (VecN t)
    MatT :: Rep t -> N -> N -> Rep (MatN t)

data Binding :: * -> * where
    Var :: String -> Rep t -> Binding t
    Swiz :: Binding v -> String -> Binding x
    Proc :: String -> Rep r -> a -> Binding (a -> r)
    FragCoord :: Binding (VecN Float)
    FragColor :: Binding (VecN Float)

data Decl :: * -> * where
    Value :: Binding t -> Expr t -> Decl t
    Uniform :: Binding t -> Maybe (Expr t) -> Decl t
    Procedure :: Binding (a -> r) -> Stmt -> Decl (a -> r)
    Function :: Binding (a -> r) -> Expr (a -> r) -> Decl (a -> r)

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
    Lam :: Binding t -> Expr u -> Expr (t -> u)
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
