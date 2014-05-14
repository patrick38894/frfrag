{-# Language OverloadedStrings #-}
------------------------------------------------------------------------------
-- Adapted from Rendering.hs (c) Svenne Panne 2013
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
import Graphics.UI.GLFW as GLFW
import System.Exit ( exitWith, ExitCode(..) )
import qualified Data.ByteString as B
import qualified Graphics.Rendering.OpenGL as GL

type Color      = GL.Color4 GLclampf
color           = Color4

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


initResources :: B.ByteString -> IO Descriptor
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
    currentProgram              $= Just program

    let vPos                    = AttribLocation 0
    vertexAttribPointer vPos    $=
        (ToFloat, VertexArrayDescriptor 2 Float 0 (bufferOffset 0))
    vertexAttribArray vPos      $= Enabled

    return $ Descriptor triangles 0 (fromIntegral screenVerts)


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

drawInWindow :: B.ByteString -> Color -> Window -> IO ()
drawInWindow fragSource bgcolor win = do
    descriptor <- initResources fragSource
    onDisplay bgcolor win descriptor

closeWindow :: Window -> IO ()
closeWindow win = do
    GLFW.destroyWindow win
    GLFW.terminate

onDisplay :: Color -> Window -> Descriptor -> IO ()
onDisplay bgcolor win descriptor@(Descriptor triangles firstIndex numVertices) = do
  GL.clearColor $= bgcolor
  GL.clear [ColorBuffer]
  bindVertexArrayObject $= Just triangles
  drawArrays Triangles firstIndex numVertices
  GLFW.swapBuffers win
  forever $ do
     GLFW.pollEvents
     onDisplay bgcolor win descriptor
