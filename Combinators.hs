{-# Language FlexibleInstances, GADTs, TypeSynonymInstances #-}

module Combinators where
import Fragment
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

-- Region constructors
data Region where
    Anywhere        :: Region
    Predicate       :: Expr (VecN Float -> Bool) -> Region
    Complement      :: Region -> Region
    Intersect       :: Region -> Region -> Region
    Union           :: Region -> Region -> Region
    Warp            :: Region -> (Expr (VecN Float -> VecN Float)) -> Region
    Difference      :: Region -> Region -> Region
    Rotate          :: Region -> Expr Float -> Region
    Transpose       :: Region -> Expr (VecN Float) -> Region
    Scale           :: Region -> Expr (VecN Float) -> Region

-- Region destructor which returns a single predicate
region :: Region -> Expr (VecN Float -> Bool)
region r = let v4 = Var "regionLAMBDA" vec4 in case r of
    Anywhere        -> Lift $ const true
    Predicate p     -> p
    Complement p    -> v4 \> notE (region p \$ Val v4)
    Intersect p q   -> v4 \> (region p \$ Val v4) .&& (region q \$ Val v4)
    Union p q       -> v4 \> (region p \$ Val v4) .|| (region q \$ Val v4)
    Warp p w        -> v4 \> region p \$ w \$ Val v4
    Difference p q  -> region $ Intersect p (Complement q)
    Rotate p theta  -> region $ Warp p (rotate theta)    
    Transpose p d   -> region $ Warp p (transpose d)
    Scale p s       -> region $ Warp p (scale s)

rotate :: Expr Float -> Expr (VecN Float -> VecN Float)
rotate (Float theta) = scale . Vec $ pointXY (cos theta) (sin theta)

transpose :: Expr (VecN Float) -> Expr (VecN Float -> VecN Float)
transpose p = let v4 = Var "LAMBDAtranspose" vec4 in v4 \> (p + Val v4)

scale :: Expr (VecN Float) -> Expr (VecN Float -> VecN Float)
scale s = let v4 = Var "LAMBDAscale" vec4 in v4 \> (s * Val v4)

-- Rectangle defined by size.
rectangle :: Expr (VecN Float) -> Region
rectangle r = let v4 = Var "rectangleLAMBDA" vec4 in
    Predicate (v4 \> (zero .> sX v4 .&& zero .> sY v4 
                 .&& gX r .< sX v4 .&& gY r .< sY v4))

-- Ellipse defined by center and foci
ellipse :: Expr (VecN Float) -> Expr (VecN Float) -> Region
ellipse c r = let v = Var "ellipseLAMBDA" vec4 in
    Predicate (v \> (one .> ((gX c/gX r)**two + (gY c/gY r)**two)))

-- Circle defined by center and radius
circle :: Expr (VecN Float) -> Expr Float -> Region
circle c r = ellipse c (pointXYE r r)

-- Triangle defined by 3 points
triangle :: Expr (VecN Float) -> Expr (VecN Float) -> Expr (VecN Float) -> Region
triangle = undefined

-- Polygon defined by a list of points
polygon :: [Expr (VecN Float)] -> Region
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
