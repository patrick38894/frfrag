{-# Language FlexibleContexts, GADTs, MultiParamTypeClasses #-}

module Interpret where

import Expressions
import Region
import Utility
import Statements
import Vector

import Control.Monad.State
import Data.Maybe

instance REq (Decl a) (Decl b)

type Interpret = State Fragment
data Fragment where Fragment :: Env -> Scope -> Region -> Fragment
type Scope = Stmt
data Env where
    Empty :: Env
    Extend :: Decl a -> Env -> Env    

lookUpDecl :: Decl a -> Env -> Maybe (Decl a)
lookUpDecl d e = case e of
    Empty -> Nothing
    Extend d' e' -> case d ~~ d' of 
                    Just Refl -> Just d'
                    Nothing -> lookUpDecl d e'

lookUpD :: Decl a -> Interpret (Maybe (Decl a))
lookUpD d = do
    Fragment e _ _ <- get
    return $ lookUpDecl d e

lookupName :: String -> Interpret (Maybe (Binding a))
lookupName = undefined

extend :: (Pretty a, Wrap Expr a, Wrap Rep a) => Decl a -> Interpret (Decl a)
extend decl = do
    alreadyDefined <- lookUpD decl
    case alreadyDefined of
        Nothing -> do
            withState (\(Fragment e s r) -> Fragment (Extend decl e) s r) (return decl)
        Just decl -> error $ "Redeclaration of " ++ show decl

check :: Interpret Fragment
check = undefined

newName :: Interpret String
newName = undefined

getVarsInScope :: Scope -> Env -> Interpret Env
getVarsInScope = undefined

getUniforms :: Env -> Interpret Env
getUniforms = undefined

interpret :: Interpret Fragment -> Fragment
interpret = undefined

declval :: (Wrap Rep a, Wrap Expr a, Pretty a) => (Binding a -> Maybe (Expr a) -> Decl a) -> 
           Binding a -> Maybe (Expr a) -> Interpret (Expr a)
declval f (v @ (Var tp nm)) def = do
    alreadyDefined <- lookupName nm
    case alreadyDefined of
        Nothing -> extend (f v def)
        Just b -> error $ "Redeclaration of " ++ show b
    return (Val v)
       
uniform :: (Wrap Expr a, Wrap Rep a, Pretty a) => Binding a -> Maybe (Expr a) -> Interpret (Expr a)
uniform = declval Uniform
 
value :: (Wrap Rep a, Wrap Expr a, Pretty a) => Expr a -> Interpret (Expr a)
value e = do
    name <- newName
    declval (\b d -> Value b (fromJust d)) (Var (wrap $ extract e) name) (Just e)

function :: Expr (a -> b) -> Interpret (Expr (a -> b))
function = undefined

setColor :: Expr (VecN Float) -> Interpret Fragment
setColor = undefined


