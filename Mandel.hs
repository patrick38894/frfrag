import Monad
import Language
import PrettyPrint
import Num
complexMult :: Expr
complexMult = undefined

colormap :: Float -> Float -> Expr -> Expr
colormap = undefined

calcMandelbrot :: Interpret Expr
calcMandelbrot = function vec2t [vec2p "p", floatp "t", intp "i"] calc
    where calc = undefined

main = putStrLn $ show (mandelbrot 1 2)

mandelbrot c1 c2 =   do
    zoom    <- uniform "zoom" vec2t (Just $ vec [4, 4])
    screen  <- uniform "screen" vec2t (Just $ vec [1280, 960])
    center  <- uniform "center" vec2t (Just $ vec [0, 0])
    step    <- uniform "step" float (Just 0.01)
    thresh  <- uniform "thresh" float (Just 8)
    iter    <- uniform "iter" int (Just 1000)
    offset  <- function vec2t $ do
                p <- param vec2t
                ret $ p - screen / 2
    scale   <- function vec2t $ do
                p <- param vec2t
                ret $ p * zoom / screen + center
    brot    <- calcMandelbrot
    colors  <- function vec4t $ do
                x <- param float
                ret $ colormap c1 c2 x
    cb      <- colors brot
    fragColor $ cb [scale . offset $ fragCoord, iter, step]
    fragColor $ sin(fragCoord) / cos fragCoord
    return cb
