module FRP where
import Language
type Uniform = (Type, Tagged)

data Input

data ControlEvent

type Fragment = [TagDecl]

data Beh a = Const a

data Event a = Event (Maybe a)


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
renderFrags fs us hz (x,y) = undefined


data Window = Window 
            { handle :: GLFW.Window,
              uniforms :: Consumer [Uniform] IO (),
              fragList :: Consumer [Fragment] IO (),
              windowControl :: Consumer [ControlEvent] IO (),
              userInput :: Producer Input IO ()
              }
