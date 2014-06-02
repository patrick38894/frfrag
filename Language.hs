{-# Language DeriveFunctor, FlexibleInstances, GADTs, StandaloneDeriving #-}
module Language where
import Vector
import Control.Monad.Free
import Swiz
------------------------------------------------------------------------------

class Functor repr => Expr repr where
    void    :: repr ()
    bool    :: Bool -> repr Bool
    int     :: Int -> repr Int
    float   :: Float -> repr Float
    vec     :: Vec v => v (repr a) -> repr (v a)
    mulOp   :: String -> repr a -> repr a -> repr a
    addOp   :: String -> repr a -> repr a -> repr a
    compOp  :: String -> repr a -> repr a -> repr Bool
    prim    :: String -> repr a -> repr a
    prim2   :: String -> repr a -> repr a -> repr a
    call    :: repr (a -> b) -> repr a -> repr b

class Functor stmt => Stmt stmt where
    set     :: decl a -> stmt next -> stmt next
    ifElse  :: repr Bool -> stmt a -> stmt a -> stmt a -> stmt next -> stmt next
    caseOf  :: [(Int, stmt a)] -> stmt a -> stmt next
    for     :: repr Int -> repr (Int -> Bool) -> repr (Int -> Int) -> stmt a -> stmt a -> stmt next
    while   :: repr Bool -> stmt a -> stmt a -> stmt next
    break   :: stmt ()
    cont    :: stmt ()
    ret     :: repr a -> stmt a 
    halt    :: stmt ()
    discard :: stmt ()
    noOp    :: stmt () -> stmt next
