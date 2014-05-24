module V2.TypeCheck where

import Control.Monad.State

------------------------------------------------------------------------------
-- Type and structure checking -----------------------------------------------

class Check a where
    check :: a -> State Env (Rep a)

-- Execution environment, used to keep the GLSL output sane in the printer ---
data Env where
    Empty :: Env
    Extend :: String -> Rep t -> t -> Env -> Env

-- Starting environment for specialized GL statements,
-- containing "unsafe" GL operations.
gl_IO_env = undefined

-- Check instances
instance Check Rep where
    check r = case r of x -> undefined

instance Check Exp where
    check e = case e of x -> undefined


