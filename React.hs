{-# Language FlexibleInstances, TypeSynonymInstances #-}
module React where
import Render
import Language
import Pipes
import Pipes.Concurrent
import qualified Data.ByteString.Char8 as B
import qualified Pipes.Prelude as P
import Clock
import Printer
import Control.Monad(forever)

-- UNFINISHED
-- Unfortunately, I did not finish this file sufficiently
-- to provide good tests or examples.
--
-- The idea here is to implement FRP primitives (similar to SOE) on Pipes

------------------------------------------------------------------------------
-- FRP infrastructure
type Time = Double

-- Datatypes for aggregating input events
data Identifier = KeyID Char
data Toggle = Up | Down
data InputEvent = KeyEvent Toggle
                | MousePos (Double, Double)
                | Resize (Int, Int)

-- Effects get a list of events (which may be empty) at every tick,
-- and may or may not change something they're hooked up to.
type Event a = Pipe ([InputEvent],Time) (Maybe a) IO ()
-- Behaviors get a list of events (which may be empty) at every tick,
-- and output the current state of some thing.
type Behavior a = Pipe ([InputEvent],Time) a IO ()

------------------------------------------------------------------------------
-- Combinators
-- Relay along the events and time unchanged
idP :: Behavior ([InputEvent], Time)
idP = cat
-- Relay along just time
time :: Behavior Time
time = lift1 snd idP
-- Relay along just events
events :: Behavior [InputEvent]
events = lift1 fst idP

-- Lifts
-- Const value
lift0 :: a -> Behavior a
lift0 x = forever $ await >> yield x

-- Function on behavior
lift1 :: (a -> b) -> Behavior a -> Behavior b
lift1 f a = a >-> (forever $ await >>= yield . f)

-- Binary combination of behaviors
lift2 :: (a -> b -> c) -> Behavior a -> Behavior b -> Behavior c
lift2 f a b = forever $ do
    i <- await
    let a' = yield i >-> a
        b' = yield i >-> b
    P.zipWith f a' b'

-- Relay along a behavior and also time
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
  where updateLoop x = do
            y <- await
            if y /= x then yield (Just y) else yield Nothing
            updateLoop y
       

-- Switch behaviors when the event e occurs
switch :: Behavior a -> Event (Behavior a) -> Behavior a
switch a e = forever $ do
    i <- await
    yield i >-> e >-> do
        b <- await
        case b of
            Nothing -> yield i >-> a
            Just a' -> yield i >-> a'

-- unfinished functions below
-- I ran out of time and energy for these,
-- but I would imagine they're fairly straightforward
-- (based on the other FRP primitives here)

-- Step, not defined yet
step :: a -> Event a -> Behavior a
step = undefined

-- StepAccum, same definition via Step as in SOE
stepAccum :: a -> Event (a -> a) -> Behavior a
stepAccum a e = b
    where b = a `step` (e `snapshot` b =>> uncurry ($))

-- withElem, not defined yet
withElem :: Event a -> [b] -> Event (a,b)
withElem = undefined

-- withElem, same definition via withAccum as in SOE
withElem_ :: Event a -> [b] -> Event b
withElem_ e bs = e `withElem` bs =>> snd

-- Snapshot, not defined yet
snapshot :: Event a -> Behavior b -> Event (a, b)
snapshot = undefined

-- Snapshot_, same definition via snapshot as in SOE
snapshot_ :: Event a -> Behavior b -> Event b
snapshot_ e b = e `snapshot` b =>> snd

-- Predicate, not implemented yet
predicate :: Behavior Bool -> Event ()
predicate = undefined

-- Inputs
-- Implementing these is a matter of wrangling data structures - 
-- straightforward, but not done yet
keyE :: Event Char
keyE = undefined

resizeE :: Event (Int, Int)
resizeE = undefined

mouseE :: Event (Float, Float)
mouseE = undefined

mouse :: Behavior (Float, Float)
mouse = (0,0) `step` mouseE

winSize :: (Int, Int) -> Behavior (Int, Int)
winSize (x,y) = (x,y) `step` resizeE

-- Operators for combining events
(.|.) :: Event a -> Event a -> Event a
e1 .|. e2 = undefined

(=>>) :: Event a -> (a -> b) -> Event b
a =>> f = forever $ do
    e <- await
    yield e >-> a >-> (await >>= \m -> yield (fmap f m))

(->>) :: Event a -> b -> Event b
a ->> x = a =>> const x

($*) :: Behavior (a->b) -> Behavior a -> Behavior b
ff $* fb = lift2 ($) ff fb

------------------------------------------------------------------------------
-- Num instances
instance Num a => Num (Behavior a) where
    (+) = lift2 (+)
    (*) = lift2 (*)
    negate = lift1 negate
    abs = lift1 abs
    signum = lift1 signum
    fromInteger = lift0 . fromInteger

instance Fractional a => Fractional (Behavior a) where
    (/) = lift2 (/)
    fromRational = lift0 . fromRational

instance Floating a => Floating (Behavior a) where
    pi = lift0 pi
    sqrt = lift1 sqrt
    exp = lift1 exp
    log = lift1 log
    sin = lift1 sin
    cos = lift1 cos
    tan = lift1 tan
    asin = lift1 asin
    acos = lift1 acos
    atan = lift1 atan
    sinh = lift1 sinh
    cosh = lift1 cosh
    tanh = lift1 tanh
    acosh = lift1 acosh
    asinh = lift1 asinh
    atanh = lift1 atanh

------------------------------------------------------------------------------
-- Interfacing with fragment shader programs,
-- all of which must have the same uniform inputs.
--

type Uniform = ((Type, Tagged), String)
type Fragment = WriteProg ()

-- Given a fragment, a behavior for its uniform parameters,
-- window title and size, and frame rate,
-- update the shader every tick with the uniforms
-- resulting from aggregating the inputs and running the behavior.

renderReactive :: Fragment -> Behavior [Uniform]
              -> String -> (Int, Int) -> Double -> IO ()
renderReactive fs ubs nm (x,y) hz = do
    let uts = map (fst . fst) (getUniforms fs)
    (ks, rsz, ms) <- drawWithInput [pprint fs] nm (x,y)
    (outp, inp) <- spawn Unbounded
    forkIO $ do runEffect $ ks >-> keyToEvent >-> toOutput outp
                performGC
    forkIO $ do runEffect $ rsz >-> rszToEvent >-> toOutput outp
                performGC
    forkIO $ do runEffect $ ms >-> msToEvent >-> toOutput outp
                performGC
    runEffect $ waitClock hz (fromInput inp) >-> ubs >-> setUniforms uts

-- Put together a list of uniform behaviors into one behavior
catUniforms :: [Behavior Uniform] -> Behavior [Uniform]
catUniforms = foldr (lift2 (:)) (lift0 [])

-- Print out a fragment and pack it into a bytestring
pprint :: Fragment -> B.ByteString
pprint = B.pack . show

-- Verify that the uniform behavior produced the right types,
-- then set each uniform
setUniforms :: [Type] -> Consumer [Uniform] IO ()
setUniforms ts = forever $ do
    us <- await
    if verifyUniforms ts us then return () else error "Wrong uniform types"
    lift $ mapM_ setUniform us

-- Complicated GLFW stuff to marshall uniforms. Ran out of time right here.
setUniform :: Uniform -> IO ()
setUniform p = undefined 

-- Turn GLFW key/resize/mouse events into aggregated events of the UserInput type
keyToEvent = undefined
rszToEvent = undefined
msToEvent = undefined

-- Get all of the uniforms associated with a fragment
getUniforms :: Fragment -> [Uniform]
getUniforms = undefined

-- Check that a list of types and a list of uniforms correspond
verifyUniforms :: [Type] -> [Uniform] -> Bool
verifyUniforms ts us = and $ zipWith (==) (map (fst . fst) us) ts
