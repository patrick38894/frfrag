{-# Language RankNTypes #-}
module React where
import Render
import Language
import Pipes
import Pipes.Concurrent

import qualified Pipes.Prelude as P
import Clock
import Control.Monad.Identity

------------------------------------------------------------------------------
-- FRP infrastructure
type Time = Double


data Toggle = Up | Down
data Identifier = KeyID Char
type Input a = Producer InputEvent IO ()
data InputEvent = KeyEvent Toggle
                | MousePos (Double, Double)
                | Resize (Int, Int)

-- Effects get a list of events (which may be empty) at every tick,
-- and may or may not change something they're hooked up to.
type Event a = Pipe ([InputEvent],Time) (Maybe a) Identity ()
-- Behaviors get a list of events (which may be empty) at every tick,
-- and output the current state of some thing.
type Behavior a = Pipe ([InputEvent],Time) a Identity ()

------------------------------------------------------------------------------
-- Combinators

idP :: Behavior ([InputEvent], Time)
idP = cat

time :: Behavior Time
time = lift1 snd idP

events :: Behavior [InputEvent]
events = lift1 fst idP

lift0 :: a -> Behavior a
lift0 x = await >> yield x

lift1 :: (a -> b) -> Behavior a -> Behavior b
lift1 f a = a >-> (await >>= yield . f)

withTime :: Behavior a -> Behavior (a, Time)
withTime a = forever $ do
    (is,t) <- await
    yield (is, t) >-> (a >-> do
        x <- await
        yield (x, t))

-- Keep track of the previous frame and output Nothing unless things change
updatesOnly :: Eq a => Behavior a -> Event a
updatesOnly a = do
    i <- await
    yield i >-> (a >-> do
        x <- await
        yield (Just x)
        updateLoop x)

updateLoop x = do
    y <- await
    if y /= x then yield (Just y) else yield Nothing
    updateLoop y
        

zipSync :: Behavior (a, Time) -> Behavior (b, Time) -> Behavior ((a,b), Time)
zipSync = undefined


------------------------------------------------------------------------------
-- Interfacing with fragment shader programs,
-- all of which must have the same uniform inputs.
--           uniform types -> uniform behavior -> shader selection
--         -> window title -> size -> reactive program
data Uniform
data Fragment
renderReactive :: [Type] -> Behavior [Uniform] -> Behavior [Fragment]
              -> String -> (Int, Int) -> IO ()

renderReactive uts ubs fbs nm (x,y) = undefined
