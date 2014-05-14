module Main where

import NGL.Shape
import NGL.Rendering
import Data.ByteString as B

main :: IO ()
main = do
    fragSource <- B.readFile "Shaders/Mandelbrot.frag"
    shadeWindow fragSource "TestWindow" (1000, 1000)

shadeWindow :: B.ByteString -> String -> (Int, Int) -> IO ()
shadeWindow fragSource title (h,w) = do

    -- Replace this with hard-coded
    vertSource <- B.readFile "Shaders/Simple.vert"
    win <- createWindow title (h,w)
    drawInWindow vertSource fragSource win [shape (Square (-1,-1) 4)]
    closeWindow win
