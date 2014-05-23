import V1.Mandelbrot
import Data.ByteString.Char8
import Common.Rendering

-- main = maindelbrotV1

-- The following three shaders do the same thing.
-- They _are_ different, which will be important later.
--
-- GLFW/OpenGL evidently runs an empty string as a shader
-- which does nothing and doesn't complain.
emptyShader = ""
-- Discard stops the shader computation immediately.
discardShaderGLSL = "void main(){ discard; }"
-- Return terminates the current scope (within a function that returns void),
-- but the shader continues to evaluate.
terminateShaderGLSL = "void main() { return; }"
-- 
redShaderGLSL = ""
regionShaderGLSL = ""
mandelbrotShaderGLSL = ""


main = testBStr terminateShaderGLSL

testBStr s = do
    Prelude.putStrLn s
    shadeWindowDef [pack s] "TestWindow" (1280, 960)


