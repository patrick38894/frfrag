import Build
import Language
import PrettyPrint
import Num
complexMult :: Expr
complexMult = undefined

colormap :: Float -> Float -> Expr -> Expr
colormap = undefined

calcMandelbrot = procedure vec2t calc
    where calc = undefined

main = print (mandelbrotEnv 1 2)

mandelbrotEnv c1 c2 =   do
    zoom    <- uniform vec2t "zoom"     (Just $ vec [4, 4])
    screen  <- uniform vec2t "screen"   (Just $ vec [1280, 960])
    center  <- uniform vec2t "center"   (Just $ vec [0, 0])
    step    <- uniform float "step"     (Just 0.01)
    thresh  <- uniform float "thresh"   (Just 8)
    iter    <- uniform int  "iter"     (Just 1000)
    offset  <- procedure vec2t $ do
                p <- param vec2t
                ret $ p - screen / 2
    scale   <- procedure vec2t $ do
                p <- param vec2t
                ret $ p * zoom / screen + center
    brot    <- calcMandelbrot
    colors  <- procedure vec4t $ do
                x <- param float
                ret $ colormap c1 c2 x
    let cb  = composeF colors brot
    mandelbrotMain <- procedure void $ do
        setColor $ cb [scale [offset [fragCoord]], iter, step]
        --setColor $ sin(fragCoord) / cos fragCoord
    return cb
