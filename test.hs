module Main where
import PrettyTest
import Data.ByteString.Char8 as B
import Rendering
import Interpret

mandelGLSL = do
    mandelbrotGLSL <- Prelude.readFile "Mandelbrot.frag"
    testBStr mandelbrotGLSL

testBStr s = do
    Prelude.putStrLn s
    shadeWindowDef [pack s] "TestWindow" (1280, 960)

red = testBStr (makeSource redProgram)
xy = testBStr (makeSource xyGradients)
main = xy
