module Render where
import Pipes
import Language


type Uniform = (Type, Tagged)

data Input

type Fragment = [TagDecl]

data Beh a = Const a

data Event a = Event (Maybe a)


-- If given, update the shader to a new one, compiling and linking the new shader program.
updateShader :: Consumer String IO ()
updateShader = undefined

-- If given, update all supplied uniform values to new ones.
updateUniforms :: Consumer [Maybe Uniform] IO ()
updateUniforms = undefined

-- Given a frequency, produce a stream of doubles counting by the period corresponding to it.
clock :: Double -> Producer Double IO ()
clock = undefined

-- Get user input from GLFW,
--  including mouse, keyboard, window resize and close events
getUserInput :: Producer Input IO ()
getUserInput = undefined

-- Wait until a clock signal before passing along next value
waitClock :: Producer Double IO () -> Producer a IO r -> Producer a IO r
waitClock = undefined


-- Render a sequence of fragment shaders, all sharing the same uniform values,
-- at a given clock rate and default window size
renderFrags :: Event [Fragment] -> Event [Uniform] -> Double -> (Double, Double) -> IO ()
renderFrags = undefined
