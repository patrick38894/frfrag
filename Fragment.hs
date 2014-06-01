module Fragment where
import Control.Monad.State
import Control.Applicative
import qualified Data.Map as M
import Region
import Language
import Env
import Procedure

type Build = State Fragment


procedureE       :: Rep -> BuildF a -> Env ([Expr] -> Expr)
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
