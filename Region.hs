{-# Language GADTs #-}
module Region where
import Expressions
import Operators
import Vector
import Utility
import Synonyms
import HigherOrder

data Region where
    Anywhere        :: Region
    Predicate       :: Expr (VecN Float -> Bool) -> Region
    Complement      :: Region -> Region
    Intersect       :: Region -> Region -> Region
    Union           :: Region -> Region -> Region
    Warp            :: Region -> (Expr (VecN Float -> VecN Float)) -> Region
    Difference      :: Region -> Region -> Region
    Rotate          :: Region -> Expr Float -> Region
    Transpose       :: Region -> Expr (VecN Float) -> Region
    Scale           :: Region -> Expr (VecN Float) -> Region

drawRegion :: Region -> Expr (VecN Float -> Bool)
drawRegion r = case r of
    Anywhere        -> liftE (const true)
    Predicate p     -> p
--    Complement p    -> notE \. region p 
--    Intersect p q   -> v4 \> (region p \$ Val v4) .&& (region q \$ Val v4)
--    Union p q       -> v4 \> (region p \$ Val v4) .|| (region q \$ Val v4)
--    Warp p w        -> region p \. w
    Difference p q  -> drawRegion $ Intersect p (Complement q)
    Rotate p theta  -> drawRegion $ Warp p (rotate theta)    
    Transpose p d   -> drawRegion $ Warp p (transpose d)
    Scale p s       -> drawRegion $ Warp p (scale s)

rotate :: Expr Float -> Expr (VecN Float -> VecN Float)
--rotate (Float theta) = scale . Vec $ pointXY (cos theta) (sin theta)
rotate = undefined

transpose :: Expr (VecN Float) -> Expr (VecN Float -> VecN Float)
--transpose p = let v4 = Var "transposeLAMBDA" vec4 in v4 \> (p + Val v4)
transpose = undefined

scale :: Expr (VecN Float) -> Expr (VecN Float -> VecN Float)
-- scale s = let v4 = Var "scaleLAMBDA" vec4 in v4 \> (s * Val v4)
scale = undefined

rectangle :: Expr (VecN Float) -> Region
--rectangle r = let v4 = Var "rectangleLAMBDA" vec4 in
--    Predicate (v4 \> (zero .> sX v4 .&& zero .> sY v4 
--                 .&& gX r .< sX v4 .&& gY r .< sY v4))
rectangle = undefined

ellipse :: Expr (VecN Float) -> Expr (VecN Float) -> Region
--ellipse c r = let v = Var "ellipseLAMBDA" vec4 in
--    Predicate (v \> (one .> ((gX c/gX r)**two + (gY c/gY r)**two)))
ellipse = undefined


-- Make shapes data, then make predicate funcs for outline and fill
-- triangle
-- polygon

-- Useful predicates :: Shape -> (VecN Float -> Bool)
-- Outline (draw a shape with a thickness)
-- Fill (draw within a curve with a thickness)
