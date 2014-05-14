module Main where

import NGL.Shape
import NGL.Rendering

main :: IO ()
main = shadeWindow "TestWindow" (1000, 1000)

shadeWindow :: String -> (Int, Int) -> IO ()
shadeWindow title (h,w) = do
    win <- createWindow title (h,w)
    drawInWindow win [shape (Square (-1,-1) 4)]
    closeWindow win
