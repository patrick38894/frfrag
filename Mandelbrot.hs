import CoreLanguage
import HigherOrder
import Operators
import Interpret
import Vector
import Utility

complexMult :: Expr (VecN Float -> VecN Float -> VecN Float)
complexMult = undefined

colormap :: Float -> Float -> Expr (Float -> VecN Float)
colormap = undefined

calcMandelbrot :: Expr (VecN Float -> Int -> Float)
calcMandelbrot = undefined

-- Generate names for uniforms, declarations.
mandelbrot :: Fragment
mandelbrot = interpret $ do
    zoom    <- uniform vec2 . Just $ Vec2 4 4
    screen  <- uniform vec2 . Just $ Vec2 1280 960
    center  <- uniform vec2 . Just $ Vec2 0 0
    step    <- uniform float $ Just 0.01
    thresh  <- uniform float 8
    iter    <- uniform int 1000
--    p       <- lamTerm vec2
--    offset  <- function $ p \> p - screen / 2
--    scale   <- function $ p \> p * zoom / screen + center
--    brot    <- function  calcMandelbrot
--    colors  <- function $ colormap 1 2
--    fragColor $ colors \$ brot \$ (scale \. offset \$ fragCoord) iter step
    return Empty [] 
