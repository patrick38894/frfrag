{-# Language
        FlexibleInstances,
        GADTs,
        TypeSynonymInstances #-}

module V2.Combinators where
import V2.Fragment
import Control.Monad.State
import Text.PrettyPrint.HughesPJ (render, vcat)

------------------------------------------------------------------------------
-- Structured GLSL program representation ------------------------------------

-- This file focuses on a AST representation of GLSL,
-- and a pretty-printer to make a GLSL source string given such an AST.

-- A fragment shader source string
--  consists of a list of uniform variable declarations
--      (the interface to the GL program)
--  a list of declarations
--      (structures, values, and functions),
--  a series of tests determining what region to shade,
--  and a main function, which runs at least one statement and returns "void".
data FragSrc = Frag
                {
                    uniform :: HetList Uniform,
                    out :: HetList Out,
                    decls :: HetList Decl,
                    regionToShade :: Region,
                    fragMain :: Stmt Void
                }

-- Fragment source files are printed by concatenating the sections.
instance PP FragSrc where
    pp (Frag us os ds rs mn) = 
        vcat [pp us, pp os, pp ds, ppmain (shadeRegion (region rs) mn)]

instance Show FragSrc where show = render . pp

------------------------------------------------------------------------------
-- Region combinators --------------------------------------------------------

type Point = (Float, Float)

fragCoord = Val FragCoord
sX c = Val (Swiz c "x")
sY c = Val (Swiz c "y")
sZ c = Val (Swiz c "z")
sW c = Val (Swiz c "w")
coordX = sX FragCoord
coordY = sY FragCoord
someVec4 = Var "someVar" vec4

-- Region constructors
data Region where
    Anywhere :: Region
    Predicate :: Expr (VecN Float -> Bool) -> Region
    Complement :: Region -> Region
    Intersect :: Region -> Region -> Region
    Union :: Region -> Region -> Region
    Difference :: Region -> Region -> Region
    Warp :: Region -> (Expr (VecN Float -> VecN Float)) -> Region
    Rotate :: Region -> Float -> Region
    Transpose :: Region -> Point -> Region
    Scale :: Region -> Point -> Region

-- Region destructor which returns a single predicate
region :: Region -> Expr (VecN Float -> Bool)
region r = let v4 = Val someVec4 in case r of
    Anywhere -> Lam someVec4 (Bool True)
    Predicate p -> p
    Complement p -> Lam someVec4 (Not (App (region p) v4))
    Intersect p q -> Lam someVec4 (And (App (region p) v4) (App (region q) v4))
    Union p q -> Lam someVec4 (Or (App (region p) v4) (App (region q) v4))
    Difference p q -> region $ Intersect p (Complement q)
    Warp p w -> Lam someVec4 (App (region p) (App w v4))
    Rotate p theta -> region $ Warp p (rotate theta)    
    Transpose p d -> region $ Warp p (transpose d)
    Scale p s -> region $ Warp p (scale s)

rotate :: Float -> Expr (VecN Float -> VecN Float)
rotate theta = scale (cos theta, sin theta)

transpose :: Point -> Expr (VecN Float -> VecN Float)
transpose (x,y) = Lam someVec4 (Vec (Vec4 x y 1 1) + Val someVec4)

scale :: Point -> Expr (VecN Float -> VecN Float)
scale (x,y) = Lam someVec4 (Vec (Vec4 x y 1 1) * Val someVec4)

-- Rectangle defined by size.
rectangle :: Point -> Region
rectangle (w, h) = let v = someVec4 in
    Predicate (Lam v ((And (And (GreaThan (Float 0) (sX v))
                                 (GreaThan (Float 0) (sY v)))
                            (And (LessThan (Float w) (sX v))
                                 (LessThan (Float h) (sY v))))))

-- Ellipse defined by center and foci
ellipse :: Point -> Point -> Region
ellipse (x,y) (a,b) = let v = Var "v" vec4 in
    Predicate (Lam v (Float 1 `GreaThan` ((Float x/Float a)**(Float 2) 
                                        + (Float y/Float b)**(Float 2))))

-- Circle defined by center and radius
circle :: Point -> Float -> Region
circle c r = ellipse c (r, r)

-- Triangle defined by 3 points
triangle :: Point -> Point -> Point -> Region
triangle = undefined


-- Polygon defined by a list of points
polygon :: [Point] -> Region
polygon = undefined

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

-- Show instance
instance Show Eval where show = show . eval

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
-- keeping all of the uniforms and statements of the first environment
-- In case of name collisions, use changeNames to change instances of the name
-- in the first environment.
mergeDecls :: Eval -> Eval -> Eval
mergeDecls = undefined

-- Change a name to avoid collision.
-- This does a deep change of declaration and value calls.
-- It ignores lambda/application, which don't ever happen in real GLSL
changeNames :: Eval -> String -> String -> Eval
changeNames = undefined

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
shadeRegion r s = IfElse (App r fragCoord) s Discard

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
