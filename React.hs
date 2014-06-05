{-# Language FlexibleInstances, TypeSynonymInstances #-}
module React where
import Render
import Language
import Pipes
import Pipes.Concurrent
import qualified Data.ByteString as B
import qualified Pipes.Prelude as P
import Clock
import Control.Monad.Identity

------------------------------------------------------------------------------
-- FRP infrastructure
type Time = Double


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

idP :: Behavior ([InputEvent], Time)
idP = cat

time :: Behavior Time
time = lift1 snd idP

events :: Behavior [InputEvent]
events = lift1 fst idP

lift0 :: a -> Behavior a
lift0 x = forever $ await >> yield x

lift1 :: (a -> b) -> Behavior a -> Behavior b
lift1 f a = a >-> (forever $ await >>= yield . f)

lift2 :: (a -> b -> c) -> Behavior a -> Behavior b -> Behavior c
lift2 f a b = forever $ do
    i <- await
    let a' = yield i >-> a
        b' = yield i >-> b
    P.zipWith f a' b'

-- lift3 and so forth can be defined similarly to lift2,
-- albeit with more complicated pairing/currying with multiple zips
-- (not used here)

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
       

switch :: Behavior a -> Event (Behavior a) -> Behavior a
switch a e = forever $ do
    i <- await
    yield i >-> e >-> do
        b <- await
        case b of
            Nothing -> yield i >-> a
            Just a' -> yield i >-> a'


step :: a -> Event a -> Behavior a
step = undefined

stepAccum :: a -> Event (a -> a) -> Behavior a
stepAccum a e = b
    where b = a `step` (e `snapshot` b =>> uncurry ($))

withElem :: Event a -> [b] -> Event (a,b)
withElem = undefined

withElem_ :: Event a -> [b] -> Event b
withElem_ e bs = e `withElem` bs =>> snd

snapshot :: Event a -> Behavior b -> Event (a, b)
snapshot = undefined

snapshot_ :: Event a -> Behavior b -> Event b
snapshot_ e b = e `snapshot` b =>> snd


predicate :: Behavior Bool -> Event ()
predicate = undefined

-- Inputs
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
--           uniform types -> uniform behavior -> shader selection
--         -> window title -> size -> reactive program
data Uniform
data Fragment
renderReactive :: [Fragment] -> [Type] -> Behavior [Uniform]
              -> String -> (Int, Int) -> Double -> IO ()
renderReactive fs uts ubs nm (x,y) hz = do
    -- Set up the initial window
    verifyUniforms fs uts
    (ks, rsz, ms) <- drawWithInput (map pprint fs) nm (x,y)
    (outp, inp) <- spawn Unbounded
    forkIO $ do runEffect $ ks >-> keyToEvent >-> toOutput outp
                performGC
    forkIO $ do runEffect $ rsz >-> rszToEvent >-> toOutput outp
                performGC
    forkIO $ do runEffect $ ms >-> msToEvent >-> toOutput outp
                performGC
    runEffect $ waitClock hz (fromInput inp) >-> ubs >-> setUniforms uts


pprint :: Fragment -> B.ByteString
pprint = undefined

setUniforms :: [Type] -> Consumer [Uniform] IO ()
setUniforms ts = undefined


keyToEvent = undefined
rszToEvent = undefined
msToEvent = undefined

verifyUniforms :: [Fragment] -> [Type] -> IO ()
verifyUniforms fs ts = undefined
 



