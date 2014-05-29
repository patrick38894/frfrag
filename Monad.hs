{-# Language FlexibleInstances, GADTs, TypeSynonymInstances #-}
module Monad where
import Language
import Region
import Vector
import Control.Monad.Reader

type Interpret = Reader Fragment

data Fragment = Fragment {env :: Env,
                          counter :: Int,
                          main :: Stmt (),
                          region :: Region}

data Env where
    Empty :: Env
    Extend :: Decl r () -> Env -> Env
