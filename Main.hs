module Main where

import NGL.Rendering
import Data.ByteString as B


main :: IO ()
main = do
    fragSource <- B.readFile "Shaders/Mandelbrot.frag"
    shadeWindow fragSource "TestWindow" (1000, 500)

shadeWindow :: B.ByteString -> String -> (Int, Int) -> IO ()
shadeWindow fragSource title (h,w) = do

    -- Replace this with hard-coded
    vertSource <- B.readFile "Shaders/Simple.vert"
    win <- createWindow title (h,w)
    drawInWindow vertSource fragSource (color 1 1 1 1) win
    closeWindow win
