{-# Language DeriveFunctor, FlexibleInstances, GADTs, TypeSynonymInstances #-}
module Decl where
import Language
import Control.Monad.Free

class Monad decl => Decl decl where
    uni     :: Expr repr => Maybe (repr a) -> decl a
    val     :: Expr repr => repr a -> decl a
    proc    :: Stmt stmt => stmt a -> decl a

data BuildProg a = Build a (BuildProg a)
                 | End
                 deriving (Functor, Show)

type Program = Free BuildProg


instance Decl Program where
    uni x    = undefined
