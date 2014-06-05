module Tests where
import Language
import Ops
import Printer

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

passthrough :: WriteProg ()
passthrough = fragMain (setColor (val FragColor))

brighten :: WriteProg ()
brighten = fragMain (setColor (val FragColor .* float 2))

contrast :: WriteProg ()
contrast = let c = val FragColor in fragMain (setColor (c .* c))

simpleAssignment :: WriteProg ()
simpleAssignment = do
    screen  <- uarg (vec_t 2)
    fragMain $ do
        x <- mkFloat; set x (screen .@ "x")
        setColor (val FragCoord ./ val x)

colormap :: (TagE Float -> TagE Float)  -> (TagE Float -> TagE Float) 
        -> (TagE Float -> TagE Float) -> Bind -> TagE (Mat Float)
colormap c1 c2 c3 x = vec [c1 $ val x, c2 $ val x, c3 $ val x, 1]

complexMult :: Expr expr => expr (Mat Float) -> expr (Mat Float) -> expr (Mat Float)
complexMult a b = vec [a .@ "x" .* b .@ "x" .- a .@ "y" .* b .@ "y",
                       swiz a "x" .* swiz b "y" .+ swiz a "y" .* swiz b "x"]

mandelbrot :: (TagE Float -> TagE Float)
            -> (TagE Float -> TagE Float)
            -> (TagE Float -> TagE Float) 
            -> (TagE (Mat Float) -> TagE (Mat Float)) -> WriteProg ()

mandelbrot c1 c2 c3 warp = do
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
        ret   (p .* zoom / screen + center)
    colors  <- proc $ do
        x   <- bind $ arg float_t
        ret   (colormap c1 c2 c3 x)
    mandel  <- proc $ do
        c <- param (vec_t 2)
        s <- mkFloat; set s (float 0)
        z <- mkVec 2; set z (val c)
        let vz = val z
        for 0 (\i -> i .< iter) (\i -> i + 1) $ do
            set z $ complexMult vz vz + val c
            ifElse (thresh .< dot vz vz) brk noOp
            set s (val s + step); noOp
        ret (val s)

    fragMain $ do
        setColor (colors (mandel (scale (offset $ warp $ val FragCoord))))
