module Build where
import Control.Monad.State
import Control.Applicative
import qualified Data.Map as M
import Region
import Language

------------------------------------------------------------------------------
-- Environments and declarations
type Env        = State (M.Map String Decl, Int)

searchName      :: String -> Env (Maybe Decl)
searchName s    = M.lookup s <$> env

binding         :: Decl -> Bind
binding d          = case d of
    Value b _   -> b
    Uniform b _ -> b
    Procedure b _ -> b

bname           :: Bind -> String
bname b         = case b of
    Var _ s     -> s
    Func b _    -> bname b
    other       -> error $ "No user-defined name for special binding " ++ show other

name            :: Decl -> String
name            = bname . binding

declval          :: Decl -> Env Expr
declval d       = do
    (env, i)    <- get
    put (M.insert (name d) d env, i)
    return (Val (binding d))

declfunc        :: Decl -> Env ([Expr] -> Expr)
declfunc d      = do
    (env, i)    <- get
    put (M.insert (name d) d env, i)
    return (Call (binding d))

nextv           :: Env Int
nextv           = do
    (env,i)     <- get
    put (env,i+1)
    return i

env             :: Env (M.Map String Decl)
env             = fmap fst get


uniform         :: Rep -> String -> Maybe Expr -> Env Expr
uniform r s e   = do
    exists      <- searchName s
    case exists of
        Nothing -> declval (Uniform (Var r s) e)
        Just x  -> error $ unwords ["Cannot declare ", show s,
                                   " (", show x, " defined)"]

value           :: Rep -> Expr -> Env Expr
value r e       = do
    i           <- nextv
    declval (Value (Var r ("var" ++ show i)) e)
    

procedure       :: Rep -> BuildF a -> Env ([Expr] -> Expr)
procedure r bf  = do
    e           <- env
    i           <- nextv
    (rs,f)      <- buildf bf 
    declfunc (Procedure (Func (Var r ("func" ++ show i)) rs) f)

 
------------------------------------------------------------------------------
-- Uniforms and values


------------------------------------------------------------------------------
-- Functions
data Procedure = MkProc Rep [Rep] Stmt
type BuildF a = State (M.Map String Decl, Int, Procedure) a

buildf          :: BuildF a ->  Env ([Bind], Stmt)
buildf          = undefined

-- Declare a parameter, adding its type to the param list,
-- and get back a reference for use in the function.
param :: Rep -> BuildF Expr
param = undefined

-- Declare many params, getting back references.
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

-- Declare an immutable global value for a particular expression,
-- set to a sentinel and lazily initialized the first time it's used.
-- Whenever that expression is used, just use the global instead from then on.
-- 
memoF :: Expr -> BuildF Expr
memoF = undefined

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
    other -> error $ "Expected a Vec3 or Vec4"

composeF :: ([Expr] -> Expr) -> ([Expr] -> Expr) -> ([Expr] -> Expr)
composeF = undefined

------------------------------------------------------------------------------
-- Fragments
data Frag       = Frag { fenv :: Env (),
                         fmain :: BuildF (),
                         fregion :: Region }

listEnv :: M.Map String Decl -> [Decl]
listEnv = map snd . M.toAscList

emptyFrag :: Frag
emptyFrag = undefined

emptyEnv :: (M.Map String Decl, Int)
emptyEnv = (M.empty, 0)

emptyStmt :: Stmt
emptyStmt = undefined

sepUniforms :: M.Map String Decl -> ([Decl], [Decl])
sepUniforms = undefined

mkFrag :: Frag -> ([Decl], [Decl], Stmt)
mkFrag f = let
    (e,c)  = execState (fenv f) (M.empty,1)
    ((bs,m),(e',_)) = runState (buildf (fmain f)) (e,c)
    (us, gs) = sepUniforms e
    in (us, gs, shadeRegion (fregion f) m)


