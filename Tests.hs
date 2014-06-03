import Language
import Num

------------------------------------------------------------------------
-- Basic

emptyFrag :: WriteProg ()
emptyFrag = fragMain noOp

discardFrag :: WriteProg ()
discardFrag = fragMain discard

haltFrag :: WriteProg ()
haltFrag = fragMain halt

redFrag :: WriteProg ()
redFrag = fragMain (setColor $ mat [[1,0,0,0]])

gradFrag :: WriteProg ()
gradFrag = fragMain (setColor (val FragCoord ./ (1000 :: TagE Float)))

sinFrag :: WriteProg ()
sinFrag = fragMain (setColor (sin (val FragCoord ./ float 1000)))

passthrough :: WriteProg ()
passthrough = fragMain (setColor (val FragColor))

brighten :: WriteProg ()
brighten = fragMain (setColor (val FragColor .* float 2))

calcMandelbrot :: WriteProc ()
calcMandelbrot = do
        p            <- param (vec_t 2)
        iter         <- param int_t
        step         <- param float_t
        -- TODO , actual calculation
        return ()

colormap :: Float -> Float -> [Bind] -> TagE (Mat Float)
colormap c1 c2 [x] = vec [val x * lit c1, 1 - val x * lit c2, 0.5, 1]

mandelbrot :: Float -> Float -> WriteProg ()
mandelbrot c1 c2 =   do
    zoom    <- udef $ vec [4,4]
    center  <- udef $ vec [0,0]
    screen  <- udef $ vec [1280, 960]
    step    <- uni $ float 0.01
    thresh  <- udef $ float 8
    iter    <- uni $ int 1000
    offset  <- proc $ do
        p   <- arg $ vec_t 2
        ret   (p .- screen ./ float 2)
    scale   <- proc $ do
        p   <- arg $ vec_t 2
        ret   (p - screen + center)
    colors  <- proc $ do
        x   <- bind $ arg float_t
        ret   (colormap c1 c2 [x])
    mandel  <- proc calcMandelbrot
    fragMain $ do
        -- comp <- bind $ offset [FragCoord]
        --setColor (mandel [offset [FragCoord]))
