module Monad where
import Language
import Region
import Vector
import Control.Monad.State
import Control.Monad.Reader
import Data.Map (Map, empty, insertWith)
import qualified Data.Map as M
------------------------------------------------------------------------------
type Interpret = State (Fragment, Int)
type Env = Map (Int, String) Decl

newtype Fragment = Fragment (Env, Stmt, Region)

emptyFrag :: Fragment
emptyFrag = Fragment (empty, NoOp, Anywhere)

interpret :: Interpret a -> Fragment
interpret p = fst $ execState p (emptyFrag, 1)

------------------------------------------------------------------------------
env :: Fragment -> Env
env (Fragment (e,_,_)) = e

update :: (Env -> Env) -> Fragment -> Fragment
update u (Fragment (e,m,r)) = Fragment (u e, m, r)

fmain :: Fragment -> Stmt
fmain (Fragment (_,m,_)) = m

rfmain :: Fragment -> Stmt
rfmain (Fragment (_,m,r)) = shadeRegion m r

binding :: Decl -> Bind
binding d = case d of
    Value       b _ -> b
    Uniform     b _ -> b
    Procedure   b _ -> b
    Function    b _ -> b

name :: Decl -> String
name = bname . binding

bname :: Bind -> String
bname b = case b of
    Void -> error "Cannot actually bind to Void"
    Var b s -> s
    FragCoord -> "gl_FragCoord"
    FragColor -> "gl_FragColor"
 

check :: Bind -> Interpret Expr
check b = do
    (frag, counter) <- get
    return (Val b)
   
declval ::  (Bind -> Maybe a -> Decl) -> Bind -> Maybe a -> Interpret Expr
declval f b x = do
    (frag, counter) <- get
    let exists = lookUpName (bname b) (env frag)
    case exists of 
        Nothing -> do
            put (update (M.insert (counter, bname b) (f b x)) frag, counter + 1)
        Just y -> error $ "Redefinition of " ++ show y
    check b

lookUpName :: String -> Env -> Maybe Bind
lookUpName s e = 
    let r = M.filterWithKey (\(_,k) _ -> k == s) e
    in if M.null r 
        then Nothing 
        else case M.assocs r of
                [(k,a)] -> Just (binding a)
                xs -> error $ "Multiple conflicting definitions " ++ (unlines $ map show xs)

valueF :: Bind -> Maybe Expr -> Decl
valueF b e = case e of
    Just x -> Value b x
    Nothing -> error $ "Value " ++ show b ++  " uninitialized"

funcF :: Bind -> Maybe Expr -> Decl
funcF b e = case e of
    Just x -> Function b x
    Nothing -> error $ "Function " ++ show b ++ " uninitialized"

procF :: Bind -> Maybe Stmt -> Decl
procF b s = case s of
    Just s -> Procedure b s
    Nothing -> error $ "Procedure " ++ show b ++ " uninitialized"

genArgs as = zipWith Var as (map (("arg"++) . show) [1..])
------------------------------------------------------------------------------
uniform :: String -> Rep -> (Maybe Expr) -> Interpret Expr
value :: Rep -> Expr -> Interpret Expr
function :: Rep -> [Rep] -> Expr -> Interpret Expr
procedure :: Rep -> [Rep] -> Stmt -> Interpret Expr

uniform s r = declval Uniform (Var r s)
value b e = do
    (_, counter) <- get
    let name = "var" ++ show counter
    declval valueF (Var b name) $ Just e
function b as e = do
    (_, counter) <- get
    let name = "func" ++ show counter
    declval funcF (Func (Var b name) (genArgs as)) $ Just e
procedure b as s = do
    (_, counter) <- get
    let name = "proc" ++ show counter
    declval procF (Func (Var b name) (genArgs as)) $ Just s
------------------------------------------------------------------------------
asProcedure :: Fragment -> Interpret Stmt
-- Add necessary uniforms
-- Set aside the "main" as a separate function
asProcedure = undefined

fuse :: (Stmt -> Stmt -> Stmt) -> Stmt -> Interpret ()
fuse f s = do
    (Fragment (e,m,r), counter) <- get
    let m' = f m s
        frag' = Fragment (e,m',r)
    put (frag', counter)

fuseblock :: Stmt -> Stmt -> Stmt
fuseblock a b = case b of
    Seq xs -> Seq (a:xs)
    other -> Seq [a, b]

firstDo :: Stmt -> Interpret ()
firstDo = fuse fuseblock

thenDo :: Stmt -> Interpret ()
thenDo = fuse (flip fuseblock)

fragColor :: Expr -> Interpret ()
fragColor x = do
    (Fragment (e,m,r), _) <- get
    thenDo (Mut FragColor x)
