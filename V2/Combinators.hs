module V2.Combinators where
import V2.Fragment

------------------------------------------------------------------------------
-- Combinators for expressions and statements --------------------------------



andThen :: (PP a, PP b, GetRep a, GetRep b) => Stmt a -> Stmt b -> Stmt Void
a `andThen` b = Block (HCons a (HCons b HNil))


------------------------------------------------------------------------------
-- Combinators for checking and composing programs ---------------------------
-- class ChecksOut 

emptyShader :: FragSrc
emptyShader = Frag HNil HNil HNil Discard

-- At the end of the main function, shade a particular given expression.
shadeExpr :: (PP a, GetRep a) => Expr a -> FragSrc -> FragSrc
shadeExpr e f = f {fragMain = fragMain f `andThen` Return e}
