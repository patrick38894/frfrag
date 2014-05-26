{-# Language FlexibleContexts, GADTs, KindSignatures #-}
module Statements where
import Expressions
import HigherOrder
import Pretty

instance Extract Decl where 
  extract e = case e of
    Value name expr -> undefined
    Uniform bind def -> undefined
    --Procedure proc stmt -> undefined
    --Function func expr -> undefined

data Decl :: * -> * where
    Value :: Binding t -> Expr t -> Decl t
    Uniform :: Binding t -> Maybe (Expr t) -> Decl t
    Procedure :: Binding (a -> r) -> Stmt -> Decl (a -> r)
    Function :: Binding (a -> r) -> Expr (a -> r) -> Decl (a -> r)


data Stmt where
    Block :: [Stmt] -> Stmt
    DecVar :: (Pretty t, Wrap Expr t, Wrap Rep t) => Binding t -> Expr t -> Stmt
    Mutate :: (Pretty t, Wrap Expr t, Wrap Rep t) => Binding t -> Expr t -> Stmt
    Switch :: Expr Int -> [(Int, Stmt)] -> Stmt
    IfElse :: Expr Bool -> Stmt -> Stmt -> Stmt
    For :: Binding Int -> Expr Int -> Expr (Int -> Bool) -> Expr (Int -> Int) -> Stmt -> Stmt
    While :: Expr Bool -> Stmt -> Stmt
    Break :: Stmt
    Continue :: Stmt
    Return :: (Pretty a, Wrap Expr a, Wrap Rep a) => Expr a -> Stmt
    Terminate :: Stmt
    Discard :: Stmt
