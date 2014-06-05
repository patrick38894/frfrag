import Render
import Tests
import Data.ByteString.Char8
import React


mbrot0 = mandelbrot (/10) (/10) (/10) id
mbrot1 = mandelbrot (\x -> 0.4 - x / 20) (/3) (\x -> let y = x/10 in y * y) id
mbrot2' = mandelbrot (/4) (\x -> 0.15 + sin x / 2) (\x -> 0.5 + x / 5)
mbrot2 = mbrot2' id
mbrot3 = mbrot2' exp
mbrot4 = mbrot2' (*2)
mbrot5 = mbrot2' (\x -> x/6 + 300)

testWin f = debugInput [pack (show f)] "test" (1000,1000)

main = do
    Prelude.putStrLn "Enter a number from 1 to 10"
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
