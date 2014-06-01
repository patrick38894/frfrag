module Env where
import Language
import Control.Monad.State
import qualified Data.Map as M

------------------------------------------------------------------------------
-- Environments and declarations
type Env        = State (M.Map String Decl, Int)

searchName      :: String -> Env (Maybe Decl)
searchName s    = fmap (M.lookup s) env

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

listEnv :: M.Map String Decl -> [Decl]
listEnv = map snd . M.toAscList

sepUniforms :: M.Map String Decl -> ([Decl], [Decl])
sepUniforms = undefined

emptyEnv :: (M.Map String Decl, Int)
emptyEnv = (M.empty, 0)
