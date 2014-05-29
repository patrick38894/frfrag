module Monad where
import Language
import Region
import Vector
import Control.Monad.Writer.Lazy
import Control.Monad.Reader
import Data.Map (Map, empty, insertWith)
import qualified Data.Map as M
------------------------------------------------------------------------------
type Interpret = WriterT Fragment (Reader (Env, Int))

type Env = Map (Int, String) Decl

newtype Fragment = Fragment ([Decl], Stmt, Region)

instance Monoid Fragment where
    mempty = emptyFrag
    mappend (Fragment (e, s, r))
            (Fragment (e', s', r')) =
                Fragment (e ++ e', s', r')


emptyFrag :: Fragment
emptyFrag = Fragment ([], NoOp, Anywhere)

decl :: Bind -> Expr -> Decl
decl = undefined

------------------------------------------------------------------------------
env :: Fragment -> [Decl]
env (Fragment (e,_,_)) = e

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
    
declval :: Bind -> Maybe Expr -> Interpret Expr
declval b x = undefined

lookUpName :: String -> Env -> Maybe Bind
lookUpName s e = 
    let r = M.filterWithKey (\(_,k) _ -> k == s) e
    in if M.null r 
        then Nothing 
        else case M.assocs r of
                [(k,a)] -> Just (binding a)
                xs -> error $ "Multiple conflicting definitions " ++ (unlines $ map show xs)

------------------------------------------------------------------------------
value :: Bind -> Expr -> Interpret Expr
uniform :: Bind -> (Maybe Expr) -> Interpret Expr
function :: Bind -> [Expr] -> Interpret Expr
procedure :: Bind -> Stmt -> Interpret Expr

value = undefined
uniform = undefined
function = undefined
procedure = undefined

