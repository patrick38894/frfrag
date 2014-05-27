-- Generate an InputEvent whenever there's an input event, and pair it with a time measurement.

-- Generate an infinite stream of clock measurements at a given sample rate.

-- Pair the InputEvent stream with the clock measurement stream,
--  zipping the clock times with Just [InputEvent] if one or more happened before the last tick,
--  or Nothing otherwise.

-- Given a behavior ([InputEvent] -> [Uniforms]):
--      Receive a stream of input events paired with time samples and generate uniforms

-- Receive a stream of time samples and events. 
--  For each incoming pair: set uniforms according to the event.

-- Behavior a: Behavior (([Maybe UserAction], [Time]) -> [a])
-- Event a: Event (([Maybe UserAction], [Time]) -> [Maybe a])

-- Instances:
--  Eq (Beh a)
--  Show (Beh a)
--  Num (Beh a)
--  Fractional
--  Floating

-- Combinators:
--  Time combinator
--  constB -> repeat x
--  =>> :: Event a -> (a -> b) -> Event b
--  ->> :: Event a -> b -> Event b
--  Lift ordinary function to Beh ($*) :: Beh (a->b) -> Beh a -> Beh b
--
--  Switch :: Beh a -> Ev (Beh a) -> Beh a
--

-- withElem :: Event a -> [b] -> Event (a, b)
-- .|. :: Evet a -> Event a -> Event a
-- snapshot :: Event a -> Behavior b -> Event (a, b)
-- step :: a -> Event a -> Behavior a
-- stepAccum :: a -> Event (a->a) -> Behavior a
--  key (Event Char)
--  lbp, rbp, mm (event point)
--      (as Behavior: mouse (Beh Float, Beh Float)
