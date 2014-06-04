module Render where
import Pipes
import Control.Monad
import qualified Pipes.Prelude as P
import Language
import System.Clock

type Uniform = (Type, Tagged)

data Input

type Fragment = [TagDecl]

data Beh a = Const a

data Event a = Event (Maybe a)


test = runEffect $ Pipes.for (clock 20) (lift . putStrLn . Prelude.show)
test2 = runEffect $ waitClock 60 (count 0) >-> P.print


count :: Monad m => Int -> Producer Int m r
count n = yield n >> count (n+1)

-- Given a frequency, produce a stream of doubles counting by the period corresponding to it.
--clock :: Double -> Producer Double IO Double
clock hz = do 
    s <- lift (monoTime)
    count 1 >-> ticks s hz

--ticks :: Double -> Pipe Double IO Double Double
ticks s hz = do
    n <- await
    t <- lift $ waitUntil $ (+s) $ (*period hz) $ fromIntegral $ n
    yield t
    ticks (s+period hz) hz

seconds :: TimeSpec -> Double
seconds (TimeSpec s ns) = fromIntegral s + fromIntegral ns / 10 ** 9

timeSpec :: Double -> TimeSpec
timeSpec d = let (s,ns) = properFraction d in
    TimeSpec s (round $ ns * 10 ** 9)

period :: Double -> Double
period hz = 1 / hz

monoTime :: IO Double
monoTime = fmap seconds (getTime Realtime)

waitUntil :: Double -> IO Double
waitUntil t = do
    t' <- monoTime
    unless (t' > t) (void $ waitUntil t)
    return t'

-- Wait until a clock signal before passing along next value
waitClock :: Double -> Producer a IO r -> Producer (Double, a) IO r
waitClock hz = P.zip (clock hz)


-- Get user input from GLFW,
--  including mouse, keyboard, window resize and close events
getUserInput :: Producer Input IO ()
getUserInput = undefined

-- If given, update the shader to a new one, compiling and linking the new shader program.
updateShader :: Consumer String IO ()
updateShader = undefined

-- If given, update all supplied uniform values to new ones.
updateUniforms :: Consumer [Maybe Uniform] IO ()
updateUniforms = undefined


-- Render a sequence of fragment shaders, all sharing the same uniform values,
-- at a given clock rate and default window size
renderFrags :: Event [Fragment] -> Event [Uniform] -> Double -> (Double, Double) -> IO ()
renderFrags = undefined
