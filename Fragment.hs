module Fragment where
import Control.Monad.State
import Control.Applicative
import qualified Data.Map as M
import Region
import Language
import Env
import Procedure

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
-- Fragments
data Frag       = Frag { fenv :: Env (),
                         fmain :: BuildF (),
                         fregion :: Region }
emptyFrag :: Frag
emptyFrag = Frag (return ()) (return ()) Anywhere

mkFrag :: Frag -> ([Decl], [Decl], Stmt)
mkFrag f = let
    (e,c)  = execState (fenv f) (M.empty,1)
    ((bs,m),(e',_)) = runState (buildf (fmain f)) (e,c)
    (us, gs) = sepUniforms e
    in (us, gs, shadeRegion (fregion f) m)
