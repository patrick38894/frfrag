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

-- Count by incrementing forever
count :: Monad m => Int -> Producer Int m r
count n = yield n >> count (n+1)

-- Measure start time, then start waiting for the time period specified
-- by the frequency argument before emitting the next measurement.
clock :: Double -> Producer Double IO r
clock hz = do 
    s <- lift (monoTime)
    count 1 >-> ticks s hz

-- After receiving an incoming number, wait until that many multiples 
-- of the time period have passed before returning the next measurement.
--ticks :: Double -> Double -> Pipe Double IO r -- but it's not this?
ticks :: Integral a => Double -> Double -> Proxy () a () Double IO r
ticks s hz = do
    n <- await
    t <- lift $ waitUntil $ (+s) $ (*period hz) $ fromIntegral $ n
    yield t
    ticks (s+period hz) hz

-- Get time from the monotonic system clock
monoTime :: IO Double
monoTime = fmap seconds (getTime Monotonic)

-- Given a time to wait until, loop until that time is just past
waitUntil :: Double -> IO Double
waitUntil t = do
    t' <- monoTime
    unless (t' > t) (void $ waitUntil t)
    return t'

-- Given some signal, combine it with the clock signal.
zipWithClock :: (Double -> a -> b) -> Double -> Producer a IO r -> Producer b IO r
zipWithClock f hz = P.zipWith f (clock hz)

-- Zip with the clock by pairing
waitClock :: Double -> Producer a IO r -> Producer (a, Double) IO r
waitClock = zipWithClock (flip (,))

-- Zip with the clock and discard the time measurement
waitClock_ :: Double -> Producer a IO r -> Producer a IO r
waitClock_ = zipWithClock (\x y -> y)
