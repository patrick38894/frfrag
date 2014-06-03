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

colormap :: Float -> Float -> [Bind] -> TagE (Mat Float)
colormap c1 c2 [x] = vec [val x * lit c1, 1 - val x * lit c2, 0.5, 1]



mandelbrot :: Float -> Float -> WriteProg ()
mandelbrot c1 c2 =   do
    zoom    <- udef $ vec [4,4]
    center  <- udef $ vec [0,0]
    screen  <- udef $ vec [1280, 960]
    step    <- udef $ float 0.01
    thresh  <- udef $ float 8
    iter    <- udef $ int 1000
    offset  <- proc $ do
        p   <- arg $ vec_t 2
        ret   (p .- screen ./ float 2)
    scale   <- proc $ do
        p   <- arg $ vec_t 2
        ret   (p * zoom / screen + center)
    colors  <- proc $ do
        x   <- bind $ arg float_t
        ret   (colormap c1 c2 [x])
    mandel  <- proc $ do undefined
    fragMain $ do
        setColor (mandel (scale (offset $ val FragCoord)))
