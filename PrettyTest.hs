import Expressions
import Statements
import Primitive
import Operators
import Vector
import Text.PrettyPrint.HughesPJ

printList = mapM_ (putStrLn . render) 

types = [   pp BoolT,
            pp IntT,
            pp FloaT, 
            pp $ VecT IntT N2,
            pp $ MatT FloaT N2 N3,
            pp VoidT,
            pp PolyT,
            pp FuncT
        ]

binds = [   pp FragCoord,
            pp FragColor,
            pp $ Var IntT "foo",
            pp $ Swiz (Var (VecT IntT N3) "foo") "xy",
            pp $ Func (Var BoolT "test") (Var BoolT "arg"),
            pp $ Func (Func (Var BoolT "test2args") (Var IntT "innerArg")) (Var FloaT "outerArg"),
            pp $ Func (Func (Func (Var BoolT "test3args")
                      (Var BoolT "inner")) (Var (VecT FloaT N2) "middle")) (Var IntT "outer"),
            pp $ Func (Var IntT "higherOrder") (Func (Var BoolT "hret") (Var BoolT "harg")), 
            pp $ Func (Func (Var IntT "higherOrder2") (Var FloaT "inner"))
                      (Func (Var BoolT "hret") (Var BoolT "harg")),
            pp $ Func (Var IntT "invalid") (FragCoord),
            pp $ Void
        ]

vec2test = Vec (VecT FloaT N2) (Vec2 3.1 3.4)

exprs = [   pp $ Float 3.1,
            pp $ Int 4,
            pp $ Bool True,
            pp vec2test,
            -- pp $ Mat (MatT FloaT N3 N2) (Vec3 vec2test vec2test vec2test)
            pp $ sinE,
            pp $ App sinE vec2test,
            pp $ App (App powE (Float 2.0)) (Float 3.0),
            pp $ addE,
            pp $ vec2test + vec2test,
            pp $ Val (Var IntT "foo"),
            pp $ Call (Func (Var IntT "ret") (Var IntT "param")),
            pp $ App (Call (Func (Var IntT "ret") (Var IntT "param"))) (Int 3),
            pp $ App (App (Call (Func (Func (Var IntT "ret")
                    (Var IntT "inr")) (Var IntT "otr"))) (Int 3)) (Int 5)
        ]

decls = [   
            pp $ Value (Var IntT "foo") (Int 7),
            pp $ Value (Var BoolT "test")
                (App (Call (Func (Var BoolT "ret") (Var IntT "param"))) (Int 3)),
            pp $ Uniform (Var IntT "mouseY") Nothing,
            pp $ Uniform (Var IntT "mouseX") (Just $ Int 320),
            pp $ Procedure (Func (Var FloaT "ret") (Var FloaT "param"))
                (Return (asTypeOf (App (Prim "sin") (Val (Var FloaT "param"))) (Float undefined))),
            pp $ Procedure (Func (Var VoidT "changeRed") (Var FloaT "col"))
                (Block [Mutate (Swiz FragColor "x") (Val (Var FloaT "col")), Terminate]),
            pp $ Function (Func (Var IntT "double") (Var IntT "i"))
               (Val (Var IntT "i") * 2)
            
        ]



main = printList (types ++ binds ++ exprs ++ decls) 
