module NGL.Rendering where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Monad
import System.Exit ( exitWith, ExitCode(..) )
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import NGL.LoadShaders
import NGL.Shape


data Descriptor = Descriptor VertexArrayObject ArrayIndex NumArrayIndices


bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral


initResources :: [Vertex2 Float] -> IO Descriptor
initResources vs = do
    triangles <- genObjectName
    bindVertexArrayObject $= Just triangles

    let vertices = vs
        numVertices = length vertices

    arrayBuffer <- genObjectName
    bindBuffer ArrayBuffer $= Just arrayBuffer
    withArray vertices $ \ptr -> do
        let size = fromIntegral (numVertices * sizeOf (head vertices))
        bufferData ArrayBuffer $= (size, ptr, StaticDraw)

    program <- loadShaders [
        ShaderInfo VertexShader (FileSource "Shaders/triangles.vert"),
        ShaderInfo FragmentShader (FileSource "Shaders/triangles.frac")]
    currentProgram $= Just program

    let firstIndex = 0
        vPosition = AttribLocation 0
    vertexAttribPointer vPosition $=
        (ToFloat, VertexArrayDescriptor 2 Float 0 (bufferOffset firstIndex))
    vertexAttribArray vPosition $= Enabled

    return $ Descriptor triangles firstIndex (fromIntegral numVertices)


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
resizeWindow win w h =
    do
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


drawInWindow :: Window -> [[Point]] -> IO ()
drawInWindow win vs = do
    descriptor <- initResources $ toVertex2 vs
    onDisplay win descriptor


closeWindow :: Window -> IO ()
closeWindow win = do
    GLFW.destroyWindow win
    GLFW.terminate


onDisplay :: Window -> Descriptor -> IO ()
onDisplay win descriptor@(Descriptor triangles firstIndex numVertices) = do
  GL.clearColor $= Color4 1 0 0 1
  GL.clear [ColorBuffer]
  bindVertexArrayObject $= Just triangles
  drawArrays Triangles firstIndex numVertices
  GLFW.swapBuffers win

  forever $ do
     GLFW.pollEvents
     onDisplay win descriptor

