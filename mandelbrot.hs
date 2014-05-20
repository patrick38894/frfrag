import Fragment
import Literals

-- First iteration.
-- Straightforward translation of GLSL syntax into the corresponding AST.
-- Carries in parameters as literals.

mandelbrotFrag :: Vec2 -> Vec2 -> Vec2 -> Float -> Float -> Int -> Frag
mandelbrotFrag  
    zoom screen center step thresh iter =
       Frag { uniforms = [],
              declarations = [offset screen, 
                              scale zoom screen center,
                              complexMult,
                              mandelbrot thresh,
                              colormap],
              fragMain = GLAsgn GLFragColor
                         (LookFunc "colormap"
                            [LookFunc "mandelbrot"
                                [LookFunc "scale"
                                    [LookFunc "offset"
                                    [Swizzle FragCoord "xy"]]]])
            } 

offset screen = Func (Bind Vec2 "offset") [Bind Vec2 "p"] 
                     (Sub (LookVal "p") (Lit (LVec2 (fst screen / 2, snd screen / 2))))

scale zoom screen center = Func (Bind Vec2 "scale") [Bind Vec2 "p"]
                            (Add (Mul (LookVal "p")
                                      (Div (Lit (LVec2 zoom))
                                           (Lit (LVec2 screen))))
                                  (Lit (LVec2 center)))

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

mandelbrot thresh = Proc (Bind Float "mandelbrot")
                  [Bind Vec2 "c", Bind Int "n", Bind Float "step"]
             [DeclVar (Bind Float "sum") (Lit (LFloat 0)),
              DeclVar (Bind Vec2 "z") (LookVal "c"),
              Iter (For (Bind Int "i") (Lit (LFloat 0))
                        (LessThan (LookVal "i") (LookVal "n"))
                        (Asgn "i" (Add (Lit (LInt 1)) (LookVal "i")))
                    (Block 
                        [Asgn "z" (LookFunc "complexMult" [LookVal "z", LookVal "z"]),
                        Sel (IfElse (LessThan (Lit (LFloat thresh))
                                         (Dot (LookVal "z") (LookVal "z")))
                            Break NoOp)])),
              Return (LookVal "sum")]

ex_mandelbrot = mandelbrotFrag (4,4) (1280,960) (0,0) 0.1 4 1000
