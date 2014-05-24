{-# Language OverloadedStrings #-}
------------------------------------------------------------------------------
-- Adapted from Rendering.hs (c) Svenne Panne 2013
-- Modifications include:
--  Changed color datatype.
--  Added shadeWindow function that draws fragment shaders, in order.
------------------------------------------------------------------------------

module Rendering where

-- Local imports
import LoadShaders

-- External imports
import Control.Monad
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL as GL hiding (Color)
import Graphics.UI.GLFW as GLFW hiding (createWindow)
import System.Exit ( exitWith, ExitCode(..) )
import qualified Data.ByteString.Char8 as B
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

data Color      = Color {color :: GL.Color4 GLclampf}

-- The only thing we'll do with vertices is use them to draw the whole screen.
toVertex2 :: (Float, Float) -> Vertex2 Float
toVertex2 (x,y) = Vertex2 x y
screen          = map toVertex2 [(-1,-1), (-1,1), (1,1), (1,-1), 
                                 (-1,-1), (-1,1), (1,1), (1,-1), (-1,-1)]
screenVerts     = length screen

vertSource      = B.intercalate "\n" 
                [ "#version 430 core",
                  "layout(location = 0) in vec4 vPosition;",
                  "void main() { gl_Position = vPosition;}"]


data Descriptor = Descriptor VertexArrayObject ArrayIndex NumArrayIndices

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral


initResources :: B.ByteString -> IO (Program, Descriptor)
initResources fragSource = do
    triangles                   <- genObjectName
    bindVertexArrayObject       $= Just triangles
    arrayBuffer                 <- genObjectName
    bindBuffer ArrayBuffer      $= Just arrayBuffer
    withArray screen            $ \ptr -> do
        let size                = fromIntegral (screenVerts * sizeOf (head screen))
        bufferData ArrayBuffer  $= (size, ptr, StaticDraw)

    program                     <- loadShaders [ ShaderInfo VertexShader vertSource, 
                                                ShaderInfo FragmentShader fragSource ]

    let vPos                    = AttribLocation 0
    vertexAttribPointer vPos    $=
        (ToFloat, VertexArrayDescriptor 2 Float 0 (bufferOffset 0))
    vertexAttribArray vPos      $= Enabled

    return $ (program, Descriptor triangles 0 (fromIntegral screenVerts))


keyPressed :: GLFW.KeyCallback 
keyPressed win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown win
keyPressed _   _               _ _                     _ = return ()


shutdown :: GLFW.WindowCloseCallback
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return ()


resizeWindow :: GLFW.WindowSizeCallback
resizeWindow win w h = do
      GL.viewport   $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
      GL.matrixMode $= GL.Projection
      GL.loadIdentity
      GL.ortho2D 0 (realToFrac w) (realToFrac h) 0


createWindow :: String -> (Int, Int) -> IO Window
createWindow title (sizex,sizey) = do
    GLFW.init
    GLFW.defaultWindowHints
    Just win <- GLFW.createWindow sizex sizey title Nothing Nothing
    GLFW.makeContextCurrent (Just win)
    GLFW.setWindowSizeCallback win (Just resizeWindow)
    GLFW.setKeyCallback win (Just keyPressed)
    GLFW.setWindowCloseCallback win (Just shutdown)
    return win

drawInWindow :: [B.ByteString] -> Color -> Window -> IO ()
drawInWindow fragSources bgcolor win = do
    descriptors <- mapM initResources fragSources
    onDisplayMany bgcolor win descriptors

closeWindow :: Window -> IO ()
closeWindow win = do
    GLFW.destroyWindow win
    GLFW.terminate

onDisplay :: Color -> Window -> (Program, Descriptor) -> IO ()
onDisplay c w x = onDisplayMany c w [x]

onDisplayMany :: Color -> Window -> [(Program, Descriptor)] -> IO ()
onDisplayMany bgcolor win descriptors = do
  GL.clearColor $= bgcolor
  GL.clear [ColorBuffer]
  mapM_ drawFragment descriptors
  GLFW.swapBuffers win
  forever $ do
     GLFW.pollEvents
     onDisplayMany bgcolor win descriptors


drawFragment (program, (Descriptor triangles firstIndex numVertices)) = do
  currentProgram $= Just program
  bindVertexArrayObject $= Just triangles
  drawArrays Triangles firstIndex numVertices

shadeWindow :: [B.ByteString] -> String -> (Int, Int) -> Color -> IO ()
shadeWindow fragSources title (h,w) color = do
    win <- createWindow title (h,w)
    drawInWindow fragSources color win
    closeWindow win

shadeWindowDef :: [B.ByteString] -> String -> (Int, Int) -> IO ()
shadeWindowDef fs t d = shadeWindow fs t d (color 1 1 1 1)
