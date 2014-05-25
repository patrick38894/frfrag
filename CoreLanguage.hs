{-# Language FlexibleInstances,
             GADTs,
             PolyKinds,
             UndecidableInstances#-}

module Fragment where
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
data SwizT t = Either (VecN t) t

data Rep :: * -> * where
    BoolT :: Rep Bool
    IntT :: Rep Int
    FloaT :: Rep Float
    VecT :: Rep t -> N -> Rep (VecN t)
    MatT :: Rep t -> N -> N -> Rep (MatN t)

data Binding :: * -> * where
    Var :: String -> Rep t -> Binding t
    Proc :: String -> Rep r -> a -> Binding (a -> r)
    Swiz :: Binding (SwizT t) -> String -> Binding (SwizT t)
    FragCoord :: Binding (VecN Float)
    FragColor :: Binding (VecN Float)

data Uniform t = Uniform (Binding t) (Maybe (Expr t))
data Out t = Out (Binding t)

data Decl :: * -> * where
    Value :: Binding t -> Expr t -> Decl t
    Procedure :: Binding (a -> r) -> [Stmt] -> Decl (a -> r)

data Expr :: * -> * where
    Float :: Float -> Expr Float
    Bool :: Bool -> Expr Bool
    Int :: Int -> Expr Int
    Vec :: VecN t -> Expr (VecN t)
    Mat :: MatN t -> Expr (MatN t)
    Val :: Binding t -> Expr t
    Prim :: String -> Expr (a -> b)
    BinOp :: String -> Expr (a -> b -> c)
    Call :: Binding (t -> u) -> Expr (t -> u)
    Lift :: (Expr t -> Expr u) -> Expr (t -> u)
    Lam :: Binding t -> Expr u -> Expr (t -> u)
    App :: Expr (t -> u) -> Expr t -> Expr u

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
