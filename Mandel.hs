import Monad
import Language
import PrettyPrint
import Num
complexMult :: Expr
complexMult = undefined

colormap :: Float -> Float -> Expr
colormap = undefined

calcMandelbrot :: Expr
calcMandelbrot = undefined

main = putStrLn $ show mandelbrot

mandelbrot =   do
    zoom    <- uniform "zoom" vec2t (Just $ vec [4, 4])
    screen  <- uniform "screen" vec2t (Just $ vec [1280, 960])
    center  <- uniform "center" vec2t (Just $ vec [0, 0])
    step    <- uniform "step" float (Just 0.01)
    thresh  <- uniform "thresh" float (Just 8)
    iter    <- uniform "iter" int (Just 1000)
--    p       <- lamTerm vec2
--    offset  <- function $ p \> p - screen / 2
--    scale   <- function $ p \> p * zoom / screen + center
--    brot    <- function  calcMandelbrot
--    colors  <- function $ colormap 1 2
--    fragColor $ colors \$ brot \$ (scale \. offset \$ fragCoord) iter step
    fragColor $ sin(fragCoord) / cos fragCoord
