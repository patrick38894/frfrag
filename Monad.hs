{-# Language FlexibleInstances, GADTs, RankNTypes, TypeSynonymInstances #-}
module Monad where
import Eq
import Language
import Region
import Vector
import Control.Monad.Reader

type Interpret = Reader Fragment

data Fragment = Fragment {env :: Env,
                          counter :: Int,
                          fragMain :: Stmt (),
                          region :: Region}

data Env where
    Empty :: Env
    Extend :: Decl r () -> Env -> Env

interpret :: Interpret a -> Fragment
interpret prog = runReader (prog >> ask) emptyFrag

emptyFrag :: Fragment
emptyFrag = Fragment Empty 0 NoOp Anywhere

------------------------------------------------------------------------------
binding :: Decl r a -> Binding r a
binding d = case d of
    Value       b _ -> b
    Uniform     b _ -> b
    Procedure   b _ -> b
    Function    b _ -> b

bname :: Binding r a -> String
bname b = undefined

name :: Decl r a -> String
name = bname . binding

lookUpDecl :: Decl r a -> Env -> Maybe (Decl r a)
lookUpDecl d e = case e of
    Empty -> Nothing
    Extend d' e' -> case d ~~ d' of
        Just Refl -> Just d
        Nothing -> lookUpDecl d e'

lookUpBinding :: Binding r a -> Env -> Maybe (Binding r a)
lookUpBinding b e = case e of
    Empty -> Nothing
    Extend d e' ->  let b' = binding d in
        case b ~~ b' of
             Just Refl -> Just b
             Nothing -> lookUpBinding b e'

lookUpName :: String -> Env -> Bool
lookUpName s e = case e of
    Empty -> False
    Extend d e' -> let s' = name d in 
        s == s' && lookUpName s e'
------------------------------------------------------------------------------


