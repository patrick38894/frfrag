module Env where
import Language
import Control.Monad.RWS hiding (void)
import Vector
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

btype           :: Bind -> Rep
btype b         = case b of
    Void        -> VoidT
    FragCoord   -> VecT FloatT N4
    FragColor   -> VecT FloatT N4
    Var r _     -> r
    Func r as   -> btype r
    Swiz b s    -> swizType b s

etype           :: Expr -> Rep
etype e         = case e of
    Float _     -> FloatT
    Bool _      -> BoolT
    Int _       -> IntT
    Vec r _     -> r
    Val b       -> btype b
    Call b _    -> btype b
    Prim r _ _ _  -> r
    Prim2 r _ _ _ _ _ -> r
    BinOp r _ _ _ _ _ -> r

swizType        :: Bind -> String -> Rep
swizType        = undefined

isUniform :: Decl -> Bool
isUniform d = case d of
    Uniform _ _ -> True
    other -> False

retType         :: Stmt -> Either EnvErr Rep
retType stmt    = case stmt of
    Loc _ _     -> Right void
    Mut _ _     -> Right void
    Seq s       -> allSameOrVoid s
    If _ i e    -> allSameType [i,e]
    Case xs     -> allSameType (map snd xs)
    For _ _ _ _ s -> retType s
    While _ s   -> retType s
    Break       -> Right void
    Cont        -> Right void
    Ret e       -> Right (etype e)
    Halt        -> Right void
    Discard     -> Right void
    NoOp        -> Right void

allSameOrVoid   :: [Stmt] -> Either EnvErr Rep
allSameOrVoid [] = Right void
allSameOrVoid xs = snd $ foldr f acc xs
  where acc      = (False, Right void)
        f x (halted, t) = let r = retType x in case halted of
            True    -> case r of
                        Right VoidT -> (True, Right VoidT)
                        other       -> (True, Left $ ExpectedReturn r)
            False   -> case x of
                        Halt -> (True, if t == Right VoidT
                            then t else Left $ ExpectedReturn r)
                        other -> (False, if r == Right VoidT 
                                        || t == r || r == Right PolyT
                            then t else Left $ RetUnification t r)

allSameType     :: [Stmt] -> Either EnvErr Rep 
allSameType xs  = foldr1 f (map retType xs)
    where f x a = if x == a then a else Left $ RetUnification a x

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
                | ExpectedReturn (Either EnvErr Rep)
                | RetUnification (Either EnvErr Rep) (Either EnvErr Rep)
                | MutUnification Expr Expr
                | OtherErr String
                deriving (Eq, Show)

type RunEnv     = RWST Env [Decl] Int (Either EnvErr)

searchName      :: String -> RunEnv (Maybe Decl)
searchName s    = asks $ M.lookup s

withDecl d f    = local (M.insert (name d) d) $ do
                    tell [d]
                    return $ f d

validate        :: Expr -> RunEnv Expr
validate        = return

declVal         :: Decl -> RunEnv Expr
declVal d       = withDecl d (Val . binding) >>= validate

declFunc        :: Decl -> RunEnv ([Expr] -> Expr)
declFunc d      = withDecl d (Call . binding)

nextv           :: (Monad m, Monoid b) => RWST a b Int m Int
nextv           = state (\i -> (i, i+1))

------------------------------------------------------------------------------
-- High level declarations
uniform         :: Rep -> Maybe Expr -> RunEnv Expr
uniform r e     = do
    i           <- nextv
    declVal (Uniform (Var r ("uni" ++ show i)) e)

global           :: Rep -> Expr -> RunEnv Expr
global r e       = do
    i           <- nextv
    declVal (Value (Var r ("var" ++ show i)) e)
    

procedure       :: RunProc () -> RunEnv ([Expr] -> Expr)
procedure f     = do
    i           <- nextv
    BldProc (as,s) <- buildProc f
    case retType (Seq s) of
        Right r -> declFunc (Procedure (Func (Var r $ "proc" ++ show i) as) (Seq s))
        Left e -> lift (Left e)

------------------------------------------------------------------------------
-- Procedure subenvironment

newtype BldProc = BldProc ([Bind], [Stmt])
type RunProc    = RWST Env BldProc Int (Either EnvErr)

instance Monoid BldProc where
    mempty      = BldProc ([],[])
    mappend (BldProc (a,b)) (BldProc (c,d))
        = BldProc (mappend a c, mappend b d)

buildProc       :: RunProc () -> RunEnv BldProc
buildProc p     = do
    e           <- ask
    Right (e',BldProc (as,f)) <- return $ evalRWST (p >> ask) e 0
    local (const e') (return $ BldProc (as, f))

param           :: Rep -> RunProc Expr
param r         = do
    i           <- nextv
    let b       = Var r ("arg" ++ show i)
    tell $ BldProc ([b], [])
    return (Val b)

ret             :: Expr -> RunProc ()
ret r           = nextDo (Ret r)

nextDo          :: Stmt -> RunProc ()
nextDo s        = tell $ BldProc ([], [s])

setColor        :: Expr -> RunProc ()
setColor e      = mutate fragColor e

mutate          :: Expr -> Expr -> RunProc ()
mutate b e      = case b of
    Val (Var r s) -> if r == etype e 
                    then nextDo (Mut (Var r s) e)
                    else lift (Left $ MutUnification b e)

asStatement     :: BldProc -> Stmt
asStatement     = undefined

------------------------------------------------------------------------------
fragMain        :: RunProc () -> RunEnv [Decl]
fragMain p      = do
    BldProc ([],s) <- buildProc p
    (us,ds)     <- asks sepUniforms 
    let fmain   = Procedure (Func (Var void "main") []) (Seq s)
    tell [fmain]
    return (us ++ ds ++ [fmain])

--runFrag         :: RunEnv [Decl] -> [Decl]
runFrag r       = case evalRWST r emptyEnv 0 of
                    Right x ->  x
                    Left err -> error (show err)
