import Render
import Tests
import Data.ByteString.Char8

mbrot = show $ mandelbrot 0.2 2

main = debugInput [pack mbrot] "test" (200,200)
