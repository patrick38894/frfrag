module Main where

import Rendering
import Data.ByteString as B

main :: IO ()
main = do
    fragSources <- mapM (B.readFile . ("Shaders/"++)) ["Mandelbrot.frag"] 
    shadeWindow fragSources "TestWindow" (1000, 1000) (color 1 1 1 1)

