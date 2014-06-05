module Clock where
import Pipes
import Control.Monad
import qualified Pipes.Prelude as P
import System.Clock


------------------------------------------------------------------------------
-- Conversion functions

seconds :: TimeSpec -> Double
seconds (TimeSpec s ns) = fromIntegral s + fromIntegral ns / 10 ** 9

timeSpec :: Double -> TimeSpec
timeSpec d = let (s,ns) = properFraction d in
    TimeSpec s (round $ ns * 10 ** 9)

period :: Double -> Double
period hz = 1 / hz

------------------------------------------------------------------------------
-- Pipes based clock

count :: Monad m => Int -> Producer Int m r
count n = yield n >> count (n+1)

clock :: Double -> Producer Double IO r
clock hz = do 
    s <- lift (monoTime)
    count 1 >-> ticks s hz

--ticks :: Double -> Double -> Pipe Double IO r -- but it's not this?
ticks :: Integral a => Double -> Double -> Proxy () a () Double IO r
ticks s hz = do
    n <- await
    t <- lift $ waitUntil $ (+s) $ (*period hz) $ fromIntegral $ n
    yield t
    ticks (s+period hz) hz

monoTime :: IO Double
monoTime = fmap seconds (getTime Monotonic)

waitUntil :: Double -> IO Double
waitUntil t = do
    t' <- monoTime
    unless (t' > t) (void $ waitUntil t)
    return t'

zipWithClock :: (Double -> a -> b) -> Double -> Producer a IO r -> Producer b IO r
zipWithClock f hz = P.zipWith f (clock hz)

waitClock :: Double -> Producer a IO r -> Producer (a, Double) IO r
waitClock = zipWithClock (flip (,))

waitClock_ :: Double -> Producer a IO r -> Producer a IO r
waitClock_ = zipWithClock (\x y -> y)
