import Control.Monad.State
import Data.Map
import Region
import Language

data Frag       = Frag { fenv :: Env (),
                         fmain :: BuildF (),
                         fregion :: Region }

------------------------------------------------------------------------------
-- Utility
declname        :: Decl -> String
declname        = undefined
decltype        :: Decl -> Rep
decltype        = undefined

------------------------------------------------------------------------------
-- Environments and declarations
type Env        = State (Map String Decl)
uniform         :: Rep -> String -> Maybe Expr -> Env Expr
uniform         = undefined

value           :: Rep -> Expr -> Env Expr
value           = undefined

procedure       :: Rep -> BuildF a -> Env ([Expr] -> Expr)
procedure       = undefined

------------------------------------------------------------------------------
-- Uniforms and values


------------------------------------------------------------------------------
-- Functions
data Procedure = MkProc Rep [Rep] Stmt
type BuildF a = Env () -> State Procedure a

-- Declare a parameter, adding its type to the param list,
-- and get back a reference for use in the function.
param :: Rep -> BuildF Expr
param = undefined

-- Declare many params, gettign back references.
params :: [Rep] -> BuildF [Expr]
params = undefined

-- Declare an immutable value in the local scope,
-- and get back a reference to that value.
letF :: Expr -> BuildF Expr
letF = undefined

-- Declare many immutable values, getting back references.
letFs :: [Expr] -> BuildF [Expr]
letFs = undefined

-- Assign to a mutable value, getting back a reference to it.
--  (If it does not exist, declare it.
--   If it does, just assign to it).
setF :: Expr -> BuildF Expr
setF = undefined

-- Assign many values at a time.
setFs :: [Expr] -> BuildF [Expr]
setFs = undefined

-- Return a value,
-- setting the return type if not set,
-- and enforcing that it's consistent if it is set.
ret :: Expr -> BuildF ()
ret = undefined

-- Bake a procedure into a Decl for use in the main program.
mkCall :: Procedure -> Decl
mkCall = undefined

------------------------------------------------------------------------------
-- Fragments


