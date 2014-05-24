{-# Language 
             FlexibleInstances,
             GADTs,
             PolyKinds,
             UndecidableInstances#-}

module Fragment where

-- Imports
import Control.Monad(guard)
import Data.Maybe(isJust)
import Text.PrettyPrint.HughesPJ


------------------------------------------------------------------------------
-- GLSL language representation ----------------------------------------------

-- Void type, used to mark certain statements as having no associated types.
data Void

-- Available vector sizes (2, 3, or 4)
-- Pattern matches for vector sizes can only use these (correct) dimensions.
data N = N2 | N3 | N4 deriving (Eq, Ord)
asInt n = (case n of N2 -> 2; N3 -> 3; N4 -> 4) :: Int

-- Small "native vectors", implemented with named constructors.
-- These vectors are used for literals 
-- and have a representation in GLSL (VecT, below)
-- Since there are only three cases,
-- every approach I tried that seemed more sophisticated
-- than simply delineating them was needlessly complex.
data VecN a where 
    Vec2 :: a -> a -> VecN a
    Vec3 :: a -> a -> a -> VecN a
    Vec4 :: a -> a -> a -> a -> VecN a

-- Turn a vector into a native list and back.
vecToList :: VecN t -> [t]
vecToList v = case v of
    Vec2 a b -> [a, b]
    Vec3 a b c -> [a, b, c]
    Vec4 a b c d -> [a, b, c, d]

vecFromList :: [t] -> VecN t
vecFromList l = case l of
    [a,b] -> Vec2 a b
    [a,b,c] -> Vec3 a b c
    [a,b,c,d] -> Vec4 a b c d

zipVec :: (a -> b -> c) -> VecN a -> VecN b -> VecN c
zipVec op a b = vecFromList $ zipWith op (vecToList a) (vecToList b)

mapVec :: (a -> b) -> VecN a -> VecN b
mapVec op = vecFromList . map op . vecToList

-- Matrix type, made of a vector of vectors.
data MatN t = MatN (VecN (VecN t))

-- Num instances for vectors
instance Num a => Num (VecN a) where
    (+) = zipVec (+)
    (*) = zipVec (*)
    abs = mapVec abs
    signum = mapVec signum
    negate = mapVec negate
    fromInteger = error "Constructing vector of unknown dimension"

instance Fractional a => Fractional (VecN a) where
    (/) = zipVec (/)
    fromRational = error "Constructing vector of unknown dimension"

instance Floating a => Floating (VecN a) where
    pi = error "Constructing vector of unknown dimension"
    exp = mapVec exp
    log = mapVec log
    sin = mapVec sin
    cos = mapVec cos
    asin = mapVec asin
    acos = mapVec acos
    atan = mapVec atan
    sinh = mapVec sinh
    cosh = mapVec cosh
    asinh = mapVec asinh
    acosh = mapVec acosh
    atanh = mapVec atanh

-- Type equality witness
data Equal :: k -> k -> * where Refl :: Equal x x
-- General test for equality of types inside of a constructor f
-- Used to compare types inside of a Rep a or a Binding a.
class RepF f where (~=~) :: f a -> f b -> Maybe(Equal a b)

-- Heterogenous lists, used for lists of arguments or struct types.
-- These lists hold a consistent context type f :: * -> *
-- loaded with arbitrary element types.
-- The only operations supported on elements of these lists
-- are checking for type equality within f,
-- equality of a loaded context (f a),
-- and printing a loaded context (f a).
-- One consequence of the PP (f a) constraint
-- is that it makes the whole module much more mutually recursive,
-- since quite a few things load up a heterogenous list at some point,
-- but can only ever check it for equality or print it.
data HetList f where
    HNil :: HetList f
    HCons :: (RepF f, PP (f a), Eq (f a)) => f a -> HetList f -> HetList f

-- Eq instance for HetLists with the same f.
-- Since every (f a) in a HetList satisfies Eq,
-- they can be checked against each other one by one -
--  as soon as a ~=~ c proves that the two element types are the same.
instance Eq (HetList f) where
    HNil == HNil = True
    HCons a bs == HCons c ds = case a ~=~ c of Just Refl -> a == c && bs == ds
                                               Nothing -> False
-- HetList type synonyms
type ArgList = HetList Binding
type Args = HetList Expr
type CaseList = HetList Case
type StmtList = HetList Stmt

-- Representation types for GLSL
-- The types in GLSL are Bool, Int, Float
-- vectors of size 2, 3, and 4,
-- matrices with dimensions m, n in {2, 3, 4},
-- and structures, which have a constructor and typed fields.
-- Additionally, VoidT is used as the return type of some functions.
data Rep :: * -> * where
    BoolT :: Rep Bool
    IntT :: Rep Int
    FloaT :: Rep Float
    VecT :: Rep t -> N -> Rep (VecN t)
    MatT :: Rep t -> N -> N -> Rep (MatN t)
    StrucT :: String -> ArgList -> Rep String
    VoidT :: Rep Void

-- Shorthand for the vector types.
vec2  = VecT FloaT N2 
vec3  = VecT FloaT N3 
vec4  = VecT FloaT N4 
ivec2 = VecT IntT  N2 
ivec3 = VecT IntT  N3 
ivec4 = VecT IntT  N4 
bvec2 = VecT BoolT N2 
bvec3 = VecT BoolT N3 
bvec4 = VecT BoolT N4 

-- Class for "native values" that have a straightforward Rep.
-- The instance selected for the type selects the appropriate Rep constructor.
class GetRep t where getRep :: t -> Rep t
instance GetRep Int where getRep = const IntT
instance GetRep Bool where getRep = const BoolT
instance GetRep Float where getRep = const FloaT
instance GetRep Void where getRep = const VoidT
-- Vectors have 3 cases.
instance GetRep a => GetRep (VecN a) where
    getRep (Vec2 x y) = VecT (getRep x) N2
    getRep (Vec3 x y z) = VecT (getRep x) N3
    getRep (Vec4 x y z w) = VecT (getRep x) N4
-- Matrices can be made out of vectors
-- (though are distinct from vectors of vectors in GLSL).
instance GetRep a => GetRep (MatN a) where
    getRep (MatN vs) = case getRep vs of VecT vt n -> case vt of VecT t m -> MatT t n m 

-- The representation can be checked for equality.
-- Primitive cases are straightforward.
-- Vectors check the dimension as well as the type.
-- Structs check the name of the struct, as well as the name of every field.
-- All other cases fail.
instance RepF Rep where
    BoolT ~=~ BoolT = Just Refl
    IntT ~=~ IntT = Just Refl
    FloaT ~=~ FloaT = Just Refl
    VoidT ~=~ VoidT = Just Refl
    VecT r1 n1 ~=~ VecT r2 n2 = do guard (n1 == n2); Refl <- (r1 ~=~ r2); Just Refl
    MatT r1 n1 m1 ~=~ MatT r2 n2 m2 = do guard (n1 == n2 && m1 == m2)
                                         Refl <- (r1 ~=~ r2);  Just Refl
    StrucT nm1 as1 ~=~ StrucT nm2 as2 = do guard (nm1 == nm2 && as1 == as2); Just Refl
    r ~=~ s = Nothing

-- Name binding.
-- Variable bindings have a type and name,
-- and are used for declared values/variables, function args, etc.
-- Procedure bindings have a name, return type and argument list (which is a list of bindings). 
-- Record bindings have a name and a list of fields.
-- "Dot access" bindings take a record binding and a field name, and produce a field binding.
-- "Swizzle" bindings take a "swizzle string" such as "xy" and return those components of a vector,
-- in a scalar or a new vector.
data Binding :: * -> * where
    Var :: String -> Rep t -> Binding t
    Proc :: String -> Rep t -> ArgList -> Binding (ArgList -> t)
    Rec :: String -> ArgList -> Binding (ArgList -> String) 
    Acc :: Binding (ArgList -> String) -> String -> Binding a
    Swiz :: Binding (VecN a) -> String -> Binding b
-- Primitive bindings
    FragCoord :: Binding (VecN Float)
    FragColor :: Binding (VecN Float)

-- Uniform bindings are prefixed by the "uniform" keyword,
--  and represent an interface to an exterior program.
--  Uniform bindings may be of any type in Rep (except VoidT),
--  and may also have a default value (or not).
data Uniform t = Uniform (Binding t) (Maybe (Expr t))

-- "Out" bindings determine what variables can be used for outputs by the shader.
-- Usually, this includes at least color.
data Out t = Out (Binding t)

-- Bindings can be checked for equality by comparing the name, and all types involved.
instance RepF Binding where
    Var s1 t1 ~=~ Var s2 t2 = guard (s1 == s2) >> (t1 ~=~ t2) 
    Proc s1 t1 as1 ~=~ Proc s2 t2 as2 = do guard (s1 == s2 && as1 == as2)
                                           Refl <- (t1 ~=~ t2)
                                           Just Refl     
    Rec s1 as1 ~=~ Rec s2 as2 = guard (s1 == s2 && as1 == as2) >> Just Refl
    a ~=~ b = Nothing

-- Expressions, which include:
-- Literals (everything through Mat),
-- Statically typed Values (lookup by name),
-- Procedure Call which fills in the ArgList with a list of Exprs (of matching types).
-- Constructor which instantiates a struct.
data Expr :: * -> * where
    Float :: Float -> Expr Float
    Bool :: Bool -> Expr Bool
    Int :: Int -> Expr Int
    Vec :: VecN t -> Expr (VecN t)
    Mat :: MatN t -> Expr (MatN t)
    Val :: Binding t -> Expr t
    Call :: Binding (ArgList -> ret) -> Args -> Expr ret
    Con :: Binding (ArgList -> rec) -> Args -> Expr rec
-- Numeric exprs
-- Multiplication is elementwise for vectors, matrix mult for matrices
-- Typing is due to the possibility of adding scalar to vector, casts, etc
    Add :: (Num a, Num b, Num c) => Expr a -> Expr b -> Expr c
    Sub :: (Num a, Num b, Num c) => Expr a -> Expr b -> Expr c
    Mul :: (Num a, Num b, Num c) => Expr a -> Expr b -> Expr c
    Div :: Expr Int -> Expr Int -> Expr Int
    Mod :: Expr Int -> Expr Int -> Expr Int
    FDiv :: (Fractional a, Fractional b, Fractional c) => Expr a -> Expr b -> Expr c
    Abs :: Num a => Expr a -> Expr a
    Sgn :: Num a => Expr a -> Expr a
    Neg :: Num a => Expr a -> Expr a
-- Comparison, for scalars only
    LessThan :: (Num a, Num b) => Expr a -> Expr b -> Expr Bool
    GreaThan :: (Num a, Num b) => Expr a -> Expr b -> Expr Bool
-- Equality and logical ops
    Equals :: Expr a -> Expr b -> Expr Bool
    Or :: Expr Bool -> Expr Bool -> Expr Bool
    And :: Expr Bool -> Expr Bool -> Expr Bool
    Not :: Expr Bool -> Expr Bool
-- Floating operations
    Sqrt :: Floating a => Expr a -> Expr b
    Pow :: (Floating a, Floating b) => Expr a -> Expr b -> Expr c
    Exp :: Floating a => Expr a -> Expr b
    Log :: Floating a => Expr a -> Expr b
    Sin :: Floating a => Expr a -> Expr b
    Cos :: Floating a => Expr a -> Expr b
    Tan :: Floating a => Expr a -> Expr b
    Sinh :: Floating a => Expr a -> Expr b
    Cosh :: Floating a => Expr a -> Expr b
    Asin :: Floating a => Expr a -> Expr b
    Acos :: Floating a => Expr a -> Expr b
    Atan :: Floating a => Expr a -> Expr b
    Asinh :: Floating a => Expr a -> Expr b
    Acosh :: Floating a => Expr a -> Expr b
    Atanh :: Floating a => Expr a -> Expr b
-- Linear algebra
    Dot :: Expr (VecN t) -> Expr (VecN t) -> Expr Float
    CompMult :: Expr (MatN t) -> Expr (MatN t) -> Expr (MatN t)
-- Two "higher level" abstract expressions are also provided:
-- Lambda, and application.
-- Lambdas are not at all supported by GLSL
-- (in fact, GLSL doesn't even permit static recursion).
-- So these work by rewriting expressions.
-- As such, they lack a pretty printing instance,
-- and it is an error to try to "compile" one to a final shader.
    Lam :: Rep t -> Expr u -> Expr (t -> u)
    App :: Expr (t -> u) -> Expr t -> Expr u

-- Shorthand for Bools
true = Bool True
false = Bool False

-- Num instances for Exprs
-- These are provided for each type,
-- so that fromInteger can be overloaded.
instance Num (Expr Int) where
    (+) = Add
    (*) = Mul
    abs = Abs
    signum = Sgn
    negate = Neg
    fromInteger = Int . fromInteger

instance Num (Expr Float) where
    (+) = Add
    (*) = Mul
    abs = Abs
    signum = Sgn
    negate = Neg
    fromInteger = Float . fromInteger

instance Num a => Num (Expr (VecN a)) where
    (+) = Add
    (*) = Mul
    abs = Abs
    signum = Sgn
    negate = Neg
    fromInteger = error "Unknown vector dimension."

instance Fractional (Expr Float) where
    (/) = FDiv
    fromRational = Float . fromRational

instance Fractional a => Fractional (Expr (VecN a)) where
    (/) = FDiv
    fromRational = error "Unknown vector dimension."

instance Floating (Expr Float) where
    pi = Float pi
    exp = Exp
    log = Log
    sin = Sin
    cos = Cos
    asin = Asin
    acos = Acos
    atan = Atan
    sinh = Sinh
    cosh = Cosh
    asinh = Asinh
    acosh = Acosh
    atanh = Atanh

instance Floating a =>  Floating (Expr (VecN a)) where
    pi = error "Unknown vector dimension"
    exp = Exp
    log = Log
    sin = Sin
    cos = Cos
    asin = Asin
    acos = Acos
    atan = Atan
    sinh = Sinh
    cosh = Cosh
    asinh = Asinh
    acosh = Acosh
    atanh = Atanh
-- Declarations are:
--  Values, which bind an expression to a name.
--      In this implementation, these are automatically immutable
--      (so there are no global variables).
--  Procedures, which associate a list of statements with args and a return type.
--      Procedure bodies are a heterogenous list of statements.
--      These need to return exactly the type their return type states.    
--  Structures, which define a structure for use as a type.
data Decl :: * -> * where
    Value :: Binding t -> Expr t -> Decl t
    Procedure :: GetRep a => Binding (ArgList -> t) -> StmtList -> Decl (ArgList -> t)
    Struct :: Binding (ArgList -> String) -> Decl (ArgList -> String)

-- Statements.
-- There are many statements in GLSL.
-- A Block is a heterogenous list of statements, with a local scope.
-- DecVar allocates a mutable local variable.
-- Mutate changes a mutable local variable.
-- Switch is a "switch-case" statement, 
--      which executes one integer labeled statement based on a test expr.
-- For is a for loop with index integer, init value, test condition,
--      next value of the index, and statement to repeatedly execute.
-- While is a loop that repeatedly checks a condition
--      (which essentially has to be a local variable in the enclosing scope)
--       and executes a statement as long as the condition holds.
-- Continue and break only make sense in a For or While loop,
--      and respectively advance to the next iteration immediately,
--      and terminate the loop immediately.
-- Return, given an expression, stops the enclosing Procedure and returns the evaluated expr.
-- Terminate is "return" with no expression, for "void" functions.
-- Discard immediately terminates _the whole shader_, not just the current procedure.
data Stmt a where
    Block :: StmtList -> Stmt Void
    DecVar :: Binding t -> Expr t -> Stmt t
    Mutate :: Binding t -> Expr t -> Stmt t
    Switch :: Expr Int -> CaseList -> Stmt (CaseList)
    IfThen :: Expr Bool -> Stmt t -> Stmt t
    IfElse :: Expr Bool -> Stmt t -> Stmt t -> Stmt t
    For :: Binding Int -> Expr Int -> Expr (Int -> Bool) -> Expr (Int -> Int) -> Stmt t -> Stmt t
    While :: Expr Bool -> Stmt t -> Stmt t
    Break :: Stmt Void
    Continue :: Stmt Void
    Return :: Expr a -> Stmt a
    Terminate :: Stmt Void
    Discard :: Stmt Void

instance RepF Stmt where
    Block l1 ~=~ Block l2 = if l1 == l2 then Just Refl else Nothing

instance Eq (Stmt a) where
    Block l1 == Block l2 = l1 == l2

-- For Switch statements, a case maps an integer to a statement to execute.
data Case :: * -> * where Case :: Int -> Stmt t -> Case t

------------------------------------------------------------------------------
-- Pretty printing -----------------------------------------------------------

-- Show instances based on pp
instance PP a => Show (VecN a) where show = render . pp

-- Class for a pretty printing function, pp.
class PP x where pp :: x -> Doc

-- Frequently used class for PP and RepF
class (Eq x, GetRep x, PP x) => RepP x
instance (Eq x, GetRep x, PP x) => RepP x

-- Instances for simple things.
-- In case some function redundantly tries to print a doc, comply.
instance PP Doc where pp = id
instance PP String where pp = text
instance PP Float where pp = pp . show
instance PP Int where pp = pp . show
instance PP N where pp = pp . asInt
instance PP Bool where pp b = pp $ if b then "true" else "false"
-- The void type should never need to be printed
instance PP Void where pp = error "Printing void type."
-- Vectors and matrices are printed as comma separated lists.
instance PP a => PP (VecN a) where pp = sep . punctuate comma . map pp . vecToList
instance PP a => PP (MatN a) where 
    pp (MatN m) = pplist . concatMap vecToList . vecToList $ m
-- Heterogenous lists are printed using helper functions (defined below).
instance PP (HetList f) where
    pp = pplist . pphlist

-- Types are printed using their type name,
-- which is fixed for primitive types,
-- includes a type prefix and dimensions for vectors and matrices,
-- and depends on the declared name for structs.
instance PP (Rep a) where
    pp r = case r of
        BoolT        -> pp "bool"
        FloaT       -> pp "float"
        IntT         -> pp "int"
        VecT r n     -> getInitial r <> pp "vec" <> pp n
        MatT r n m   -> getInitial r <> pp "mat" <> pp n 
                        <> if m == n then empty else pp "x" <> pp m
        StrucT nm fs  -> pp nm 

-- Expressions include literals, value lookups, and procedure calls.
-- Additionally, there are a large number of primitives available in GLSL,
-- which are defined as special cases. 
instance (GetRep a, PP a) => PP (Expr a) where
    pp expr = case expr of
        Float n    -> pp (show n)
        Bool b     -> pp (if b then "true" else "false")
        Int n      -> pp (show n)
        Vec v      -> getInitial (getRep v) <> (parens $ pp v)
        Mat m      -> getInitial (getRep m) <> (parens $ pp m)
        Val (Var s r) -> pp s
        Call (Proc s r a) p -> pp s <> (parens (pp p))

-- Bindings include some combination of type and name.
instance PP (Binding t) where
    pp (Var s r) = pp r <+> pp s
    pp (Proc s r as) = pp r <+> pp s <> parens (pp as)
    pp (Rec s as) = pp s
    pp (Acc rec s) = pp rec <> pp "." <> pp s
    pp (Swiz vec s) = pp vec <> pp "." <> pp s

-- Uniform and out bindings just add a prefix qualifier,
-- and uniform bindings may also set a default value.
instance (GetRep t, PP t) => PP (Uniform t) where 
    pp (Uniform b d) = pp "uniform" <+> pp b <> case d of 
        Just x -> empty <+> equals <+> pp x
        Nothing -> semi
instance PP (Out t) where pp (Out b) = pp "out" <+> pp b

-- Declarations are either const values,
-- procedure prototypes and bodies (statements inside a block),
-- or structure definitions.
instance (GetRep t, PP t) => PP (Decl t) where
    pp dec = case dec of
        Value (Var s r) e -> pp "const" <+> pp r <+> pp s <+> equals <+> pp e
        Procedure (Proc s r a) b -> pp r <+> pp s <+> parens (pp a) 
                                 $+$ braceBlock (pp b)
        Struct (Rec s a) -> pp "struct" <+> pp s $+$ braceBlock (semiBind a)

-- Statements basically get laid out in sequence.
instance (GetRep a, PP a) => PP (Stmt a) where
    pp stmt = case stmt of
        Block stmts -> braceBlock (vcat (pphlist stmts))
        DecVar (Var s r) e -> pp r <+> pp s <+> equals <+> pp e <> semi
        Mutate (Var s r) e -> pp s <+> equals <+> pp e <> semi
        Switch t c -> pp "switch" <> parens (pp t) $+$ braceBlock (ppcases c)
        For ix init test iter stmt -> 
            pp "for" <> parens (pp ix <+> equals <+> pp init <> semi
                               <+> pp (App test (Val ix)) <> semi 
                               <+> pp (Mutate ix (App iter (Val ix))))
                               $+$ braceBlock (pp stmt)
        While test stmt -> pp "while" <> parens (pp test) 
                          $+$ braceBlock (pp stmt)
        Break -> pp "break" <> semi
        Continue -> pp "continue" <> semi
        Return e -> pp "return" <+> pp e <> semi
        Terminate -> pp "return" <> semi
        Discard -> pp "discard" <> semi

-- Case syntax
instance (GetRep a, PP a) => PP (Case a) where
    pp (Case c s) = pp "case" <+> pp c <+> colon $+$ braceBlock (pp s)

-- Convenience functions for pretty printers
getInitial :: Rep a -> Doc
getInitial a = text $ case a of BoolT -> "b"; IntT -> "i"; FloaT -> ""

pplist :: PP x => [x] -> Doc
pplist = sep . punctuate comma . map pp

pphlist :: HetList f -> [Doc]
pphlist HNil = []
pphlist (HCons b bs) = pp b : pphlist bs

braceBlock :: Doc -> Doc
braceBlock d = lbrace $+$ nest 4 d $+$ rbrace

ppcases :: CaseList -> Doc
ppcases = vcat . punctuate semi . pphlist

semiBind :: ArgList -> Doc
semiBind = vcat . punctuate semi . pphlist

ppmain :: Stmt Void -> Doc
ppmain mn =  pp "void main" <> parens empty 
             $+$ braceBlock (pp mn)
