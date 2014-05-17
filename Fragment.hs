{-# Language RankNTypes, ExistentialQuantification, FlexibleInstances, ImpredicativeTypes, StandaloneDeriving #-}
module Fragment where
import Literals

-- "Kind of pure" shaders - GLSL shader subset
--  (with restriction to simplify implementation - NOT for any purported advantages):
--      * just outputs a color
--      * all arguments const
--      * single assignment for globals (statements only in function bodies)
--      * no support for buffers/samplers/complex output layout/other cool GL stuff

-- The basic constructor Frag,
--  given a list of input uniform variables, a list of declarations,
--  and a main expression can be used to make a subset of fragment shaders.
--  Namely, these shaders evaluate the list of declarations in order,
--  then output a single value of type Vec4, which is the GL fragment draw color.
data Frag           = Frag {uniforms :: [Uniform], 
                            declarations :: [Decl], 
                            fragMain :: Main} deriving Show

--  Uniform inputs are basically typed names
--  (where the type is encoded in the Lit constructor)
--  and maybe a predefined value.
data Uniform        = Uni Lit String (Maybe Lit)                deriving Show


--  Expressions are either the special gl_FragCoord, 
--  a literal, or a function call or a value lookup.
--  Either way, when evaluated these will need some kind of environment lookup.
data Expr           = FragCoord
                    | LitExp Lit
                    | ExpFunc Func [Arg] 
                    | ExpVal Val

deriving instance Show Expr

--  Arguments are just a type (encoded in the Lit constructor) and a name.
--  All arguments will be treated as const.
data Arg        = forall a . (Show a) => Arg (a -> Lit) String
instance Show Arg where show (Arg f s ) = s 

--  A declaration is either a function definitions or a value.
--  Evaluating declarations should update the environment map.
--  All Vals are declared const in normal Decls.
data Decl           = DeclFunc Func
                    | DeclGlob Val                              deriving Show

--  A value is a name and an expression to evaluate and bind to that name.
--  Values are single assignment outside of functions.
data Val            = Val String Expr                           deriving Show

--  A function is a return type and an argument list,
--  then a list of statements to execute.
data Func           = Func (forall a . a -> Lit) [Arg] [Stmt]                     

instance Show Func where
    show (Func rt args sts) = unwords ["Func", show args, show sts]

--  Statements, available only in functions,
--  allow declarations as well as assignments and control flow.
--  Unlike in normal Decls, declarations in statements
--  can be of a value or a variable.
data Stmt           = DeclVar String Expr
                    | DeclVal String Expr
                    | Asgn String Expr
                    | Sel (Sel [Stmt])
                    | Iter Iter
                    | Jump Jump                                 
                    | Block [Stmt]                              deriving Show

-- Selections are if ... else and switch ... case.
-- (IfElse with empty second argument is the same as If with no Else)
-- Switch has None as the default case, and Just x for case x
data Sel a          = IfElse Expr a a
                    | Switch Expr [(Maybe Int, a)]              deriving Show

-- Iterations include For, While, and DoWhile loops.
data Iter           = For Val Expr Stmt [Stmt]
                    | While Expr [Stmt]
                    | DoWhile [Stmt] Expr                       deriving Show

-- Jump statements are Continue, Break, and Return.
-- Continue and Break apply during any Iteration or during Switch.
-- Return applies anywhere, but must return an Expr that evaluates to the type of the function.
data Jump           = Continue
                    | Break
                    | Return Expr                               deriving Show

-- Main function is restricted here.
-- It can only select among other main functions,
-- set the fragment output color to an Expr evaluated in the current environment,
-- or execute a special Discard jump.
data Main           = Pick (Sel Main)
                    | SetColor Expr
                    | Discard                                   deriving Show


-- TODO
--  Consider differentiating function and procedure.
--  Really, just do it.
--
--  Use constructors instead of values as type sentinals?

------------------------------------------------------------------------------
-- Examples ------------------------------------------------------------------

-- Mandelbrot fractal
-- Parameters are scale, zoom, center, step, threshold, iterations.
mandelbrotFrag :: Vec2 -> Vec2 -> Vec2 -> Vec2 -> Float -> Float -> Int -> Frag
mandelbrotFrag          -- Big parameter list!
    scale zoom screen center step thresh iter =
       Frag { uniforms = [],
              declarations = [offset, scale, complexMult, mandelbrot, complexCoord, colormap],
              fragMain = Discard
            } 
        where
            offset = undefined
            -- Num instance would be nice
            --   offset = Func (Vec2 undefined) [] [Return (Var "p" `Subtract` screen `Div` 2)]
            scale = undefined
            --   scale = (p `ElemMult` zoom `ElemMult` scale) `Div` screen `Plus` center
            complexMult = undefined
            --  complexMult a b = xsuby (a `ElemMult` b) `Plus` (a `Dot` flipVect b)
            mandelbrot = undefined
            --  mandelbrot = Func (Vec2 undefined) [] [
            complexCoord = undefined
            colormap = undefined

ex_mandelbrot = undefined
