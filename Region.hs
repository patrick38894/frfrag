{-# Language GADTs #-}
module Region where
import Language
import Vector

data Region where
    Anywhere :: Region
    Predicate :: Expr -> Region


predicate :: Region -> Expr
predicate r = case r of
    Anywhere -> Lam 0 (VecT FloatT N4) (Bool True)
    Predicate p -> p

shadeRegion :: Stmt -> Region -> Stmt
shadeRegion s r = If (App (predicate r) [Val FragCoord]) s Discard

--xG10 :: Expr Bool (VecN Float)
--xG10 = Rewrite (Sym (VecT FloatT N4) 0) (App (Prim "sin") (Sym (VecT FloatT N4) 0))
