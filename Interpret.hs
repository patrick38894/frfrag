{-# Language GADTs #-}

module Interpret where

import CoreLanguage
import Region
import Utility

import Control.Monad.State

type Scope = [Stmt]

data Env where
    Empty :: Env
    Extend :: Decl a -> Env -> Env    

data Fragment where Fragment :: Env -> Scope -> Region -> Fragment

type Interpret = State Fragment

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

lamTerm :: Rep a -> Interpret (Expr a)
lamTerm = undefined


