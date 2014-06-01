import Env
import Language
import PrettyPrint
import Num
complexMult :: Expr
complexMult = undefined

colormap :: Float -> Float -> Expr -> Expr
colormap = undefined

calcMandelbrot = procedure calc
    where calc = undefined

mandelbrot c1 c2 =   do
    zoom    <- uniform vec2t (Just $ vec [4, 4])
    screen  <- uniform vec2t (Just $ vec [1280, 960])
    center  <- uniform vec2t (Just $ vec [0, 0])
    step    <- uniform float (Just 0.01)
    thresh  <- uniform float (Just 8)
    iter    <- uniform int  (Just 1000)
    offset  <- procedure $ do
                p <- param vec2t
                ret $ p - screen / 2
    scale   <- procedure $ do
                p <- param vec2t
                ret $ p * zoom / screen + center
    brot    <- calcMandelbrot
    colors  <- procedure $ do
                x <- param float
                ret $ colormap c1 c2 x
    let cb  = \xs -> colors [brot xs]
    fragMain $ do
        setColor $ cb [scale [offset [fragCoord]], iter, step]
