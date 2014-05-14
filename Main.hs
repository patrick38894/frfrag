module Main where

import Rendering
import Data.ByteString as B

main :: IO ()
main = do
    fragSource <- B.readFile "Mandelbrot.frag"
    shadeWindow fragSource "TestWindow" (1000, 1000)

shadeWindow :: B.ByteString -> String -> (Int, Int) -> IO ()
shadeWindow fragSource title (h,w) = do

    -- Replace this with hard-coded
    win <- createWindow title (h,w)
    drawInWindow fragSource (color 1 1 1 1) win
    closeWindow win
