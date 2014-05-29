{-# Language
        FlexibleInstances,
        GADTs,
        KindSignatures,
        TypeSynonymInstances #-}

module Language where
import Vector
------------------------------------------------------------------------------
data Rep :: * -> * -> * where
    VoidT   :: Rep () () 
    BoolT   :: Rep Bool () 
    IntT    :: Rep Int () 
    FloatT  :: Rep Float () 
    VecT    :: Rep r () -> N -> Rep (VecN r) () 
    FuncT   :: Rep b a -> Rep r b -> Rep r (a, b)
------------------------------------------------------------------------------
data Binding :: * -> * -> * where
    Void :: Binding () ()
    FragCoord :: Binding (VecN Float) ()
    FragColor :: Binding (VecN Float) ()
    Var :: Rep t () -> String -> Binding t ()
    Func :: Binding a () -> Binding r b -> Binding r (a, b)
------------------------------------------------------------------------------
data Expr :: * -> * -> * where
    Float :: Float -> Expr Float ()
    Bool :: Bool -> Expr Bool ()
    Int :: Int -> Expr Int ()
    Vec :: Eq r => Rep r () -> VecN (Expr r ()) -> Expr (VecN r) ()
    Val :: Binding r () -> Expr r ()
    Call :: Binding r a -> Expr r a
    Prim :: String -> Expr r a
    Prim2 :: String -> Expr r (a, b)
    BinOp :: String -> Expr r (a, b)
    App :: Expr r b -> Expr b () -> Expr r ()
    Lam :: Int -> Rep b () -> Expr r () -> Expr r b
    Sym :: Int -> Rep b () -> Expr b ()
------------------------------------------------------------------------------
data Decl :: * -> * -> * where
    Value :: Binding t () -> Expr r () -> Decl t ()
    Uniform :: Binding t () -> Maybe (Expr t ()) -> Decl t ()
    Procedure :: Binding r a -> Stmt r -> Decl r a
    Function :: Binding r a -> Expr r a -> Decl r a
------------------------------------------------------------------------------
data Stmt :: * -> * where
    Loc :: Binding r () -> Expr r () -> Stmt ()
    Mut :: Binding r () -> Expr r () -> Stmt ()
    Seq :: Stmt r -> Stmt s -> Stmt (r,s)
    If :: Expr Bool () -> Stmt r -> Stmt s -> Stmt (Either r s)
    For :: Binding Int () -> Expr Int () -> Expr Bool Int -> Expr Int Int 
        -> Stmt r -> Stmt r
    While :: Expr Bool () -> Stmt r -> Stmt r
    Break :: Stmt ()
    Cont :: Stmt ()
    Ret :: Expr r () -> Stmt r
    Halt :: Stmt ()
    Discard :: Stmt ()
    NoOp :: Stmt ()
------------------------------------------------------------------------------

