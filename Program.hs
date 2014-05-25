module Program where
import Control.Monad.State
import CoreLanguage
import Instances
import Region

------------------------------------------------------------------------------
-- Structured GLSL program representation ------------------------------------

data FragSrc = Frag
                {
                    uniform :: HetList Uniform,
                    out :: HetList Out,
                    decls :: HetList Decl,
                    regionToShade :: Region,
                    fragMain :: Stmt
                }


------------------------------------------------------------------------------
-- Monadic interpreter for building up programs ------------------------------

-- Valueless environment. 
data Env where
    Empty :: Env
    Extend :: Binding t -> Expr t -> Env -> Env

-- Evaluation consists of checking that the environment is sane,
-- while building up a program using combinators.
type Eval = State Env (Either FragSrc CompileError)
data CompileError = Err String deriving (Eq, Show)

-- Shorthand for the two "return" cases
success :: FragSrc -> Eval
success = return . Left
failure :: CompileError -> Eval
failure = return . Right

-- Shorthand for evaluating a "complete" environment.
eval :: Eval -> Either FragSrc CompileError
eval e = evalState e Empty


-- Check the environment to see if an Expr is legal
checkExpr :: Eval -> Expr a -> Eval
checkExpr = undefined

-- Check the environment to see if a Stmt is legal
checkStmt :: Eval -> Stmt a -> Eval
checkStmt = undefined

-- Add a declaration to the environment and see if it's still legal.
declare :: Eval -> Decl a -> Eval 
declare = undefined

-- Add a uniform to the environment
addUniform :: Eval -> Uniform a -> Eval
addUniform = undefined

-- Merge all of the uniforms among two environments,
-- keeping the declarations and statements of the first environment
-- In case of name collisions, use changeNames to change instances of the name
-- in the first environment.
mergeUniforms :: Eval -> Eval -> Eval
mergeUniforms = undefined

-- Merge all of the declarations among two environments,
mergeDecls :: Eval -> Eval -> Eval
mergeDecls = undefined

------------------------------------------------------------------------------
-- Basic combinators ---------------------------------------------------------

-- Set FragColor to a particular given expression.
shadeExpr :: Expr (VecN Float) -> Stmt (VecN Float)
shadeExpr e = Mutate FragColor e

-- Set FragColor to a literal color
shadeColor :: VecN Float -> Stmt (VecN Float) 
shadeColor c = shadeExpr $ case c of Vec3 a b c -> Vec (Vec4 a b c 1)
                                     v @ (Vec4 a b c d) -> Vec v
                                     x -> error "Wrong color dimension"

-- Shade a select region and discard the rest
shadeRegion :: Expr (VecN Float -> Bool) -> Stmt Void -> Stmt Void
shadeRegion r s = IfElse (r \$ fragCoord) s Discard

-- Execute one statement after another
andThen :: (PP a, PP b, GetRep a, GetRep b) => Stmt a -> Stmt b -> Stmt Void
a `andThen` b = Block (HCons a (HCons b HNil))

------------------------------------------------------------------------------
-- Complete shader programs --------------------------------------------------

-- Shader which does nothing
emptyShader :: Eval
emptyShader = success $ Frag HNil HNil HNil Anywhere Discard

-- Shader which executes a single statement
singleStmt :: (GetRep a, PP a) => Stmt a -> Eval
singleStmt s = emptyShader `thenDo` s

-- Check a condition, and then execute the shader if it succeeds
ifThen :: Expr Bool -> Eval -> Eval
ifThen c e = do Left src <- e
                checkExpr e c
                success $ src {fragMain = IfThen c (fragMain src)}

-- Execute the given shader a, then execute the additional statement s
thenDo :: (PP a, GetRep a) => Eval -> Stmt a -> Eval
a `thenDo` s = do Left src <- a
                  checkStmt a s
                  success $ src {fragMain = fragMain src `andThen` s } 

-- Execute the given shader a, then execute the shader b
thenShade :: Eval -> Eval -> Eval
thenShade a b = do Left c <- (b `mergeUniforms` a) `mergeDecls` a
                   a `thenDo` fragMain c


