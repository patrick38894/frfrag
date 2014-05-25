{-# Language GADTs #-}

module Interpret where

import CoreLanguage
import Region
import Utility

import Control.Monad.State

data CompileError = Err String

type Scope = [Stmt]

data Env where
    Empty :: Env
    Extend :: Decl a -> Env -> Env    

data Fragment where
    Fragment :: Env -> Scope -> Region -> Fragment

type Interpret a = State Fragment (Either a CompileError)

check :: Interpret Fragment
check = undefined

getVarsInScope :: Scope -> Env -> Interpret Env
getVarsInScope = undefined


getUniforms :: Env -> Interpret Env
getUniforms = undefined
