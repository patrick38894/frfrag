{-# Language FlexibleContexts, GADTs #-}

module Interpret where

import Expressions
import Region
import Utility
import Statements
import Vector

import Control.Monad.State
import Data.Maybe

type Interpret = State Fragment
data Fragment where Fragment :: Env -> Scope -> Region -> Fragment
type Scope = Stmt
data Env where
    Empty :: Env
    Extend :: Decl a -> Env -> Env    

lookupName :: String -> Interpret (Maybe (Binding a))
lookupName = undefined

extend :: Decl a -> Interpret (Binding a)
extend = undefined

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

declval :: (Binding a -> Maybe (Expr a) -> Decl a) -> 
           Binding a -> Maybe (Expr a) -> Interpret (Expr a)
declval f (v @ (Var tp nm)) def = do
    alreadyDefined <- lookupName nm
    case alreadyDefined of
        Nothing -> extend (f v def)
        Just b -> error $ "Redefining binding " ++ show b
    return (Val v)
       
uniform :: Binding a -> Maybe (Expr a) -> Interpret (Expr a)
uniform = declval Uniform
 
value :: Wrap Rep a => Expr a -> Interpret (Expr a)
value e = do
    name <- newName
    declval (\b d -> Value b (fromJust d)) (Var (wrap $ extract e) name) (Just e)

function :: Expr (a -> b) -> Interpret (Expr (a -> b))
function = undefined

setColor :: Expr (VecN Float) -> Interpret Fragment
setColor = undefined


