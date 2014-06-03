import Language
import Num

calcMandelbrot :: TagE (Mat Float) -> TagE Int -> TagE Float -> WriteProc ()
calcMandelbrot o i s = do
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
    step    <- udef $ float 0.01
    thresh  <- udef $ float 8
    iter    <- udef $ int 1000
    offset  <- proc $ do
        p   <- arg $ vec_t 2
        ret   (p .- screen ./ float 2)
    scale   <- proc $ do
        p   <- arg $ vec_t 2
        ret   (p - screen + center)
    colors  <- proc $ do
        x   <- bind $ arg float_t
        ret   (colormap c1 c2 [x])
    fragMain $ do undefined
    -- setColor (calcMandelbrot (offset [FragCoord]) iter step)
