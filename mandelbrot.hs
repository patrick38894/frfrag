module Main where
import Fragment
import Literals
import Rendering
import Data.ByteString.Char8 as B

ex_mandelbrot_packed :: ByteString
ex_mandelbrot_packed = pack (show ex_mandelbrot)
ex_mandelbrot = mandelbrotFrag (4,4) (1280,960) (0,0) 0.01 8 1000
main :: IO ()
main = do
    Prelude.putStrLn (show ex_mandelbrot)
    shadeWindow [ex_mandelbrot_packed] "TestWindow" (1280, 960) (color 1 1 1 1)

-- First iteration.
-- Straightforward translation of GLSL syntax into the corresponding AST.
-- Carries in parameters as literals.

mandelbrotFrag :: Vec2 -> Vec2 -> Vec2 -> Float -> Float -> Int -> Frag
mandelbrotFrag  
    zoom screen center step thresh iter =
       Frag { uniforms = [Uni (Bind Vec2 "zoom") (Just (LVec2 zoom)),
                          Uni (Bind Vec2 "screen") (Just (LVec2 screen)),
                          Uni (Bind Vec2 "center") (Just (LVec2 center)),
                          Uni (Bind Float "step") (Just (LFloat step)),
                          Uni (Bind Float "thresh") (Just (LFloat thresh)),
                          Uni (Bind Int "iter") (Just (LInt iter))],
              declarations = [offset, 
                              scale,
                              complexMult,
                              mandelbrot,
                              colormap],
              fragMain = GLAsgn GLFragColor
                         (LookFunc "colormap"
                            [LookFunc "mandelbrot"
                                [LookFunc "scale" [LookFunc "offset" [Swizzle FragCoord "xy"]],
                                 LookVal "iter", LookVal "step"]])
            } 

offset = Func (Bind Vec2 "offset") [Bind Vec2 "p"] 
         (Sub (LookVal "p") (Div (LookVal "screen") (Lit (LFloat 2))))

scale = Func (Bind Vec2 "scale") [Bind Vec2 "p"]
                            (Add (Mul (LookVal "p")
                                      (Div (LookVal "zoom")
                                           (LookVal "screen")))
                                  (LookVal "center"))

complexMult = Func (Bind Vec2 "complexMult") [Bind Vec2 "a", Bind Vec2 "b"]
              (Con Vec2 [Sub (Mul (Swizzle (LookVal "a") "x")
                                  (Swizzle (LookVal "b") "x"))
                             (Mul (Swizzle (LookVal "a") "y")
                                  (Swizzle (LookVal "b") "y")),
                         Add (Mul (Swizzle (LookVal "a") "x")
                                  (Swizzle (LookVal "b") "y"))
                             (Mul (Swizzle (LookVal "a") "y")
                                  (Swizzle (LookVal "b") "x"))])

colormap = Func (Bind Vec4 "colormap") [Bind Float "x"]
           (Con Vec4 [Mul (LookVal "x") (Lit (LFloat 2)),
                      Sub (Lit (LFloat 1)) 
                          (Mul (LookVal "x") (Lit (LFloat 10))),
                      Lit (LFloat 0.5), Lit (LFloat 1)])

mandelbrot = Proc (Bind Float "mandelbrot") [Bind Vec2 "c", Bind Int "n", Bind Float "step"]
             [DeclVar (Bind Float "sum") (Lit (LFloat 0)),
              DeclVar (Bind Vec2 "z") (LookVal "c"),
              Iter (For (Bind Int "i") (Lit (LInt 0))
                        (LessThan (LookVal "i") (LookVal "n"))
                        (Asgn "i" (Add (Lit (LInt 1)) (LookVal "i")))

                    (Block 
                        [Asgn "z" (Add (LookFunc "complexMult" [LookVal "z", LookVal "z"]) (LookVal "c")),
                        Sel (IfElse (LessThan (LookVal "thresh")
                                         (Dot (LookVal "z") (LookVal "z")))
                                Break NoOp),
                        Asgn "sum" (Add (LookVal "sum") (LookVal "step"))])),
              Return (LookVal "sum")]

