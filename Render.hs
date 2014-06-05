{-# Language OverloadedStrings #-}
module Render where
import Clock
import Control.Exception(bracketOnError)
import Control.Monad (void,forever,unless)
import Pipes
import Pipes.Concurrent
import System.Exit (exitWith, ExitCode(..))
import Foreign.Marshal.Array(withArray)
import Foreign.Storable(sizeOf)
import Foreign.Ptr(plusPtr, nullPtr)
import Graphics.Rendering.OpenGL (($=))
import qualified Data.ByteString.Char8 as B
import qualified Graphics.UI.GLFW as GLFW
import qualified Pipes.Prelude as P
import qualified Graphics.Rendering.OpenGL as GL

------------------------------------------------------------------------------
-- Basic GLSL vertex source
vertSource = B.intercalate "\n"
           [ "#version 430 core",
             "layout(location = 0) in vec4 v;",
             "void main() { gl_Position = v; }"]

------------------------------------------------------------------------------
-- Low-level rendering
-- Taken from examples by Svenne Panne, (c) 2013

type KeyInfo = (GLFW.Key, Int, GLFW.KeyState, GLFW.ModifierKeys) 
data Descriptor = Desc GL.VertexArrayObject GL.ArrayIndex GL.NumArrayIndices 
data ShaderInfo = ShaderInfo GL.ShaderType B.ByteString

-- Initialize a GL fragment shader from source
initShader :: B.ByteString -> IO (GL.Program, Descriptor)
initShader src = do
    let w = map (\(x,y) -> GL.Vertex2 x y)
            [(-1,-1), (-1,1), (1,1), (1,-1), 
             (-1,-1), (-1,1), (1,1), (1,-1), (-1,-1::Float)]
    ts <- GL.genObjectName
    as <- GL.genObjectName
    GL.bindVertexArrayObject $= Just ts
    GL.bindBuffer GL.ArrayBuffer $= Just as
    withArray w $ \p -> do
        let s = fromIntegral $ length w * sizeOf (head w)
        GL.bufferData GL.ArrayBuffer $= (s, p, GL.StaticDraw)
    prog <- loadShader src
    let v = GL.AttribLocation 0
    GL.vertexAttribPointer v $= 
        (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 
                     (plusPtr nullPtr $ fromIntegral 0))
    GL.vertexAttribArray v $= GL.Enabled
    return $ (prog, Desc ts 0 (fromIntegral (length w)))

-- Load a fragment shader (along with the default vertex shader)
loadShader src =
    GL.createProgram `bracketOnError` GL.deleteObjectName $ \p -> do
    loadCompileAttach p [ ShaderInfo GL.VertexShader vertSource,
                          ShaderInfo GL.FragmentShader src ]
    linkAndCheck p
    return p

-- Load, compile, and attach shaders
loadCompileAttach :: GL.Program -> [ShaderInfo] -> IO ()
loadCompileAttach p [] = return ()
loadCompileAttach p (ShaderInfo ty src : is) =
    GL.createShader ty `bracketOnError` GL.deleteObjectName $ \f -> do
        GL.shaderSourceBS f $= src
        compileAndCheck f
        GL.attachShader p f
        loadCompileAttach p is

-- Compile and link shaders
compileAndCheck = checkStatus GL.compileShader GL.compileStatus GL.shaderInfoLog "compile"
linkAndCheck = checkStatus GL.linkProgram GL.linkStatus GL.programInfoLog "link"
checkStatus a g i m o = do
    a o
    s <- GL.get (g o)
    unless s (do log <- GL.get (i o); fail (m ++ " log: " ++ log))

-- Initialize a GLFW window and return a handle to it
createWindow :: String -> (Int, Int) 
            -> IO (GLFW.Window, 
                  Producer KeyInfo IO (), 
                  Producer (Int,Int) IO (),
                  Producer (Double,Double) IO ())
createWindow name (x,y) = do
    GLFW.init
    GLFW.defaultWindowHints
    Just w <- GLFW.createWindow x y name Nothing Nothing
    GLFW.makeContextCurrent (Just w)
    GLFW.setWindowCloseCallback w (Just closeWindow)
    return (w, keyPressedP w, resizeP w, cursorP w)

-- Draw each shader in a list of fragment sources in the given window
drawInWindow :: [B.ByteString] -> GLFW.Window -> IO ()
drawInWindow srcs w = do
    ds <- mapM initShader srcs
    forever $ do
        GL.clearColor $= GL.Color4 0 0 0 0
        GL.clear [GL.ColorBuffer]
        mapM_ drawFragment ds
        GLFW.swapBuffers w
        GLFW.pollEvents

-- Draw a single fragment shader in the current context
drawFragment :: (GL.Program, Descriptor) -> IO ()
drawFragment (p, (Desc ts ix vn)) = do
    GL.currentProgram $= Just p
    GL.bindVertexArrayObject $= Just ts
    GL.drawArrays GL.Triangles ix vn

-- Draw each shader. Return handles to pipes with window input.
drawWithInput :: [B.ByteString] -> String -> (Int, Int)
                -> IO (Producer KeyInfo IO (), 
                      Producer (Int,Int) IO (),
                      Producer (Double,Double) IO ())
drawWithInput fs nm (x,y) = do
    (w, keyPipe, szPipe, curPipe) <- createWindow nm (x,y)
    forkIO (drawInWindow fs w)
    return (keyPipe, szPipe, curPipe)

-- Draw each shader, and use the given input handlers to handle input.
handleInput keyHandler szHandler curHandler fs nm (x,y) = do
    (w, keyPipe, szPipe, curPipe) <- createWindow nm (x,y)
    forkIO $ runEffect $ keyPipe >-> keyHandler
    forkIO $ runEffect $ szPipe >-> szHandler
    forkIO $ runEffect $ curPipe >-> curHandler
    drawInWindow fs w

-- Print out all input to the console using show
debugInput :: [B.ByteString] -> String -> (Int, Int) -> IO ()
debugInput = handleInput P.print P.print P.print
    
-- Close the window on close signal
closeWindow :: GLFW.WindowCloseCallback
closeWindow w = do
    GLFW.destroyWindow w
    GLFW.terminate
    void (exitWith ExitSuccess)

-- Export input resize events in a pipe
resizeP :: GLFW.Window -> Producer (Int,Int) IO ()
resizeP w = do
    (outp, inp) <- lift $ spawn Single
    let f w x y = do
        atomically $ send outp (x,y)
        GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral x) (fromIntegral y))
        GL.matrixMode $= GL.Projection
        GL.loadIdentity
        GL.ortho2D 0 (realToFrac x) (realToFrac y) 0
    lift $ GLFW.setWindowSizeCallback w (Just f)
    fromInput inp

-- Export input key events in a pipe
keyPressedP :: GLFW.Window -> Producer KeyInfo IO ()
keyPressedP w = do
    (outp,inp) <- lift $ spawn Single
    let f w k s t u = atomically $ send outp (k,s,t,u) >> return ()
    lift $ GLFW.setKeyCallback w (Just f)
    fromInput inp 

-- Export mouse location
cursorP :: GLFW.Window -> Producer (Double, Double) IO ()
cursorP w = do
    (outp, inp) <- lift $ spawn Single
    let f w x y = atomically $ send outp (x,y) >> return ()
    lift $ GLFW.setCursorPosCallback w (Just f)
    fromInput inp 
