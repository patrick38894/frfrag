module Comp where
import Language
import Num

test :: WriteProc ()
test = do
    for 0 (\i -> i .< 10) (\i -> i + 1) noOp

(.<) :: (Ord a, Tag a) => TagE a -> TagE a -> TagE Bool
(.<) = compOp "<"
