{-# Language GADTs #-}

module Interpret where

import CoreLanguage
import Region
import Utility

import Control.Monad.State

type Interpret = State Fragment
data Fragment where Fragment :: Env -> Scope -> Region -> Fragment
type Scope = [Stmt]
data Env where
    Empty :: Env
    Extend :: Decl a -> Env -> Env    

check :: Interpret Fragment
check = undefined

getVarsInScope :: Scope -> Env -> Interpret Env
getVarsInScope = undefined

getUniforms :: Env -> Interpret Env
getUniforms = undefined

interpret :: Interpret Fragment -> Fragment
interpret = undefined

uniform :: Rep a -> Maybe a -> Interpret (Expr a)
uniform = undefined

function :: Expr (a -> b) -> Interpret (Expr (a -> b))
function = undefined

value :: Expr a -> Interpret (Expr a)
value = undefined

fragColor :: Expr (VecN Float) -> Interpret Fragment
fragColor = undefined


