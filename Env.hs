module Env where
import Language
import Control.Monad.RWS
import Control.Monad.Trans.Except
import qualified Data.Map as M

------------------------------------------------------------------------------
-- Util
binding         :: Decl -> Bind
binding d          = case d of
    Value b _   -> b
    Uniform b _ -> b
    Procedure b _ -> b

name            :: Decl -> String
name            = bname . binding

bname           :: Bind -> String
bname b         = case b of
    Var _ s     -> s
    Func b _    -> bname b
    other       -> error $ "No user-defined name for special binding " ++ show other

isUniform :: Decl -> Bool
isUniform d = case d of
    Uniform _ _ -> True
    other -> False

retType         :: Stmt -> Either EnvErr Rep
retType         = undefined

------------------------------------------------------------------------------
-- Map from names to declarations
type Env        = M.Map String Decl

emptyEnv        :: Env
emptyEnv        = M.empty

listEnv :: Env -> [Decl]
listEnv = map snd . M.toAscList

sepUniforms :: M.Map String Decl -> ([Decl], [Decl])
sepUniforms e = (listEnv us, listEnv ds)
 where (us, ds) = M.partition isUniform e

------------------------------------------------------------------------------
-- Declaration environment
data EnvErr     = AlreadyDefined Bind
                | OtherErr String
                deriving Show

type RunEnv     = RWST Env [Decl] Int (Either EnvErr)

searchName      :: String -> RunEnv (Maybe Decl)
searchName s    = asks $ M.lookup s

withDecl d f    = local (M.insert (name d) d) $ do
                    tell [d]
                    return $ f d

validate        :: Expr -> RunEnv Expr
validate        = return . id

declVal         :: Decl -> RunEnv Expr
declVal d       = withDecl d (Val . binding) >>= validate

declFunc        :: Decl -> RunEnv ([Expr] -> Expr)
declFunc d      = withDecl d (Call . binding)

nextv           :: RunEnv Int
nextv           = state (\i -> (i, i+1))

------------------------------------------------------------------------------
-- High level declarations
uniform         :: Rep -> Maybe Expr -> RunEnv Expr
uniform r e     = do
    i           <- nextv
    declVal (Uniform (Var r ("uni" ++ show i)) e)

value           :: Rep -> Expr -> RunEnv Expr
value r e       = do
    i           <- nextv
    declVal (Value (Var r ("var" ++ show i)) e)
    

procedure       :: RunProc () -> RunEnv ([Expr] -> Expr)
procedure f     = do
    i           <- nextv
    (as,s)      <- buildProc f
    case retType s of
        Right r -> declFunc (Procedure (Func (Var r $ "proc" ++ show i) as) s)
        Left e -> lift (Left e)
------------------------------------------------------------------------------
-- Procedure subenvironment

type RunProc    = RWST Env ([Bind],[Stmt]) Int (Either EnvErr)

buildProc       :: RunProc () -> RunEnv ([Bind],Stmt)
buildProc p     = do
    e           <- ask
    Right (e',(as,f)) <- return $ evalRWST (p >> ask) e 0
    local (const e') (return (as, Seq f))
    


