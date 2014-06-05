import Render
import Tests
import Data.ByteString.Char8
import React
import Clock
import Pipes
import qualified Pipes.Prelude as P

-- Output a bunch of timed signals with pipes/clock
clocktest1 = runEffect $ waitClock 10 (Clock.count 0) >-> P.print
clocktest2 = runEffect $ waitClock_ 5 (Clock.count 100) >-> P.print

-- Black and white mbrot
mbrot0 = mandelbrot (/10) (/10) (/10) id

-- Colored examples
mbrot1 = mandelbrot (\x -> 0.5 - x / 10) id (\x -> let y = x/10 in y * y) id
mbrot2' = mandelbrot id (\x -> 0.15 + sin x / 2) (\x -> 0.2 + x / 3)
mbrot2 = mbrot2' id

-- warp by exp -> kill the mbrot
mbrot3 = mbrot2' exp
mbrot4 = mbrot2' (*2)
mbrot5 = mbrot2' (\x -> x/6 + 300)

-- Test a shader in a window, along with debug output to the console for any input events
testWin f = debugInput [pack (show f)] "test" (1000,1000)

-- Select from and run test cases
main = do
    Prelude.putStrLn "Enter a number from 1 to 11"
    r <- Prelude.getLine 
    case read r of
        1 -> Prelude.putStrLn (show redFrag)
        2 -> Prelude.putStrLn (show mbrot1)
        3 -> testWin redFrag
        4 -> testWin gradFrag
        5 -> testWin mbrot0
        6 -> testWin mbrot1
        7 -> testWin mbrot2
        8 -> testWin mbrot3
        9 -> testWin mbrot4
        10 -> testWin mbrot5
        11 -> clocktest2
