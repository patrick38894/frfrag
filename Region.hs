{-# Language GADTs #-}
module Region where
import Language
import Vector

data Region where
    Anywhere :: Region
    Predicate :: Expr Bool (VecN Float) -> Region


predicate :: Region -> Expr Bool (VecN Float)
predicate r = case r of
--    Anywhere -> Rewrite (Sym (VecT FloatT N4) 0) (Bool True)
    Predicate p -> p

shadeRegion :: Stmt () -> Region -> Stmt (Either () ())
shadeRegion s r = If (App (predicate r) (Val FragCoord)) s Discard

--xG10 :: Expr Bool (VecN Float)
--xG10 = Rewrite (Sym (VecT FloatT N4) 0) (App (Prim "sin") (Sym (VecT FloatT N4) 0))
