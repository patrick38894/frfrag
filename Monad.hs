{-# Language FlexibleInstances, GADTs, RankNTypes, TypeSynonymInstances #-}
module Monad where
import Language
import Region
import Vector
import Control.Monad.Reader

------------------------------------------------------------------------------
type Interpret = Reader Fragment

data Fragment = Fragment {env :: Env,
                          counter :: Int,
                          fragMain :: Stmt,
                          region :: Region}

data Env where
    Empty :: Env
    Extend :: Decl -> Env -> Env

interpret :: Interpret a -> Fragment
interpret prog = runReader (prog >> ask) emptyFrag

emptyFrag :: Fragment
emptyFrag = Fragment Empty 0 NoOp Anywhere

typeE :: Expr -> Rep
typeE = undefined
------------------------------------------------------------------------------
binding :: Decl -> Bind
binding d = case d of
    Value       b _ -> b
    Uniform     b _ -> b
    Procedure   b _ -> b
    Function    b _ -> b

btype :: Bind -> Rep
btype b = case b of
    Void -> VoidT
    FragCoord -> VecT FloatT N4
    FragColor -> VecT FloatT N4
    Var b s -> b
    Func a r -> FuncT (btype a) (btype r)

bname :: Bind -> String
bname b = undefined

name :: Decl -> String
name = bname . binding

lookUpDecl :: Decl -> Env -> Maybe Decl
lookUpDecl d e = case e of
    Empty -> Nothing
    Extend d' e' -> if d == d' then Just d else lookUpDecl d e'

lookUpBinding :: Bind -> Env -> Maybe Bind
lookUpBinding b e = case e of
    Empty -> Nothing
    Extend d e' ->  let b' = binding d in
        if b == b' then Just b else lookUpBinding b e'

lookUpName :: String -> Env -> Bool
lookUpName s e = case e of
    Empty -> False
    Extend d e' -> let s' = name d in 
        s == s' && lookUpName s e'
------------------------------------------------------------------------------


