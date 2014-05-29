{-# Language FlexibleInstances, GADTs, KindSignatures, TypeSynonymInstances #-}
module Language where
import Vector
------------------------------------------------------------------------------
data Rep :: * -> * -> * where
    VoidT :: Rep () ()
    BoolT :: Rep () Bool
    IntT :: Rep () Int
    FloatT :: Rep () Float
    VecT :: Rep () a -> N -> Rep () (VecN a)
    FuncT :: Rep () a -> Rep a' r -> Rep a (Rep a' r)
------------------------------------------------------------------------------
data Binding :: * -> * -> * where
    Void :: Binding () ()
    FragCoord :: Binding () (VecN Float)
    FragColor :: Binding () (VecN Float)
    Var :: Rep () t -> String -> Binding () t
    Func :: Binding () a -> Binding a' r -> Binding a r
------------------------------------------------------------------------------
data Expr :: * -> * -> * where
    Float :: Float -> Expr () Float
    Bool :: Bool -> Expr () Bool
    Int :: Int -> Expr () Int
    Vec :: Rep () r -> VecN (Expr () r) -> Expr () (VecN r)
    Val :: Binding () r -> Expr () r
    Call :: Binding a r -> Expr a r
    Prim :: String -> Expr a r
    Prim2 :: String -> Expr a (Expr a' r)
    BinOp :: String -> Expr a (Expr a' r)
    Rewrite :: Expr a a' -> Expr a' r' -> Expr a r'
    Sym :: Rep a r -> Int -> Expr a r
    App :: Expr a' r -> Expr a a' -> Expr a r
------------------------------------------------------------------------------
data Decl :: * -> * -> * where
    Value :: Binding () t -> Expr () t -> Decl () t
    Uniform :: Binding () t -> Maybe (Expr () t) -> Decl () t
    Procedure :: Binding a r -> Stmt r -> Decl a r
    Function :: Binding a r -> Expr a r -> Decl a r
------------------------------------------------------------------------------
data Stmt :: * -> * where
    Loc :: Binding () r -> Expr () r -> Stmt ()
    Seq :: Stmt r -> Stmt s -> Stmt (r,s)
    If :: Expr () Bool -> Stmt r -> Stmt s -> Stmt (Either r s)
    For :: Binding () Int -> Expr Int Bool -> Expr Int Int -> Stmt r -> Stmt [r]
    While :: Expr () Bool -> Stmt r -> Stmt [r]
    Break :: Stmt ()
    Cont :: Stmt ()
    Ret :: Expr () r -> Stmt r
    Halt :: Stmt ()
    Discard :: Stmt ()
------------------------------------------------------------------------------
