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

main = putStrLn $ show (mandelbrot 1 2)

mandelbrot c1 c2 =   do
    zoom    <- uniform "zoom" vec2t (Just $ vec [4, 4])
    screen  <- uniform "screen" vec2t (Just $ vec [1280, 960])
    center  <- uniform "center" vec2t (Just $ vec [0, 0])
    step    <- uniform "step" float (Just 0.01)
    thresh  <- uniform "thresh" float (Just 8)
    iter    <- uniform "iter" int (Just 1000)
    p       <- symbol vec2t
    let offset = lambda p (p - screen / 2)
        scale  = lambda p (p * zoom / screen + center)
    brot    <- function vec2t [vec2t, float, int] calcMandelbrot
    colors  <- function vec4t [float] (colormap c1 c2)
    cb      <- colors \. brot
    fragColor $ cb \$ [scale . offset $ fragCoord, iter, step]
    fragColor $ sin(fragCoord) / cos fragCoord
    return cb
