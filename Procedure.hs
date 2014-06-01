module Procedure where
import Language
import qualified Data.Map as M
import Control.Monad.State
import Env

------------------------------------------------------------------------------
-- Functions
data Procedure = MkProc Rep [Rep] Stmt
type BuildF a = State (M.Map String Decl, Int, Procedure) a

buildf          :: BuildF a ->  Env ([Bind], Stmt)
buildf          = undefined

lookVal         :: Bind -> BuildF (Maybe Expr)
lookVal = do
    -- First check locals
    -- Then check params
    -- then check environment
    undefined

-- Declare a parameter, adding its type to the param list,
-- and get back a reference for use in the function.
param :: Rep -> BuildF Expr
param = undefined

-- Declare many params, getting back references.
params :: [Rep] -> BuildF [Expr]
params = mapM param

-- Declare an immutable value in the local scope,
-- and get back a reference to that value.
letF :: Expr -> BuildF Expr
letF = undefined

-- Declare many immutable values, getting back references.
letFs :: [Expr] -> BuildF [Expr]
letFs = mapM letF

-- Assign to a mutable value, getting back a reference to it.
--  (If it does not exist, declare it.
--   If it does, just assign to it).
setF :: Expr -> BuildF Expr
setF = undefined

-- Assign many values at a time.
setFs :: [Expr] -> BuildF [Expr]
setFs = mapM setF

-- Return a value,
-- setting the return type if not set,
-- and enforcing that it's consistent if it is set.
ret :: Expr -> BuildF ()
ret = undefined

-- Bake a procedure into a Decl for use in the main program.
mkCall :: Procedure -> Decl
mkCall = undefined

-- Set the GL fragment color
setColor :: Expr -> BuildF ()
setColor c = case c of
    Vec r ns -> undefined
    other -> error  "Expected a Vec3 or Vec4"

composeF :: ([Expr] -> Expr) -> ([Expr] -> Expr) -> [Expr] -> Expr
composeF = undefined


emptyStmt :: Stmt
emptyStmt = undefined
