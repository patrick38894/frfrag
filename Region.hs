{-# Language GADTs #-}
module Region where
import Language
import Vector

data Region where
    Anywhere :: Region
    Predicate :: Expr -> Region


predicate :: Region -> Expr
predicate r = case r of
    Anywhere -> true
    Predicate p -> p

shadeRegion :: Stmt -> Region -> Stmt
shadeRegion s r = If (predicate r) s Discard

--xG10 :: Expr Bool (VecN Float)
--xG10 = Rewrite (Sym (VecT FloatT N4) 0) (App (Prim "sin") (Sym (VecT FloatT N4) 0))
