{-# Language DeriveFunctor, 
             FlexibleContexts,
             FlexibleInstances,
             GADTs,
             IncoherentInstances,
             MultiParamTypeClasses, 
             RankNTypes,
             StandaloneDeriving #-}

module Language where
import Control.Monad.State
import Control.Arrow(first)

------------------------------------------------------------------------------
-- Type representations
-- Primitive types in GLSL
data PrimType = Bool | Int | Float deriving Eq
-- Types are scalar, vector, or matrix, all of which 
-- can be represented using a single type and two dimensions.
-- There is also a "void" type.
data Type = Type PrimType Int Int | Void deriving Eq
-- A binding pairs a (hopefully unique) integer identifier with a type,
-- or is one of the built-in FragCoord or FragColor
data Bind = Var Type Int | FragCoord | FragColor deriving Eq
-- If a literal can be labelled with a type and shown,
-- other type information can be discarded for the purposes of the AST.
data Tagged = forall a . (Show a, Tag a) => Tagged a

-- Tag class and instances.
class Tag a where tag :: a -> Type
instance Tag Bool where tag = const (Type Bool 1 1)
instance Tag Int where tag = const (Type Int 1 1)
instance Tag Float where tag = const (Type Float 1 1)
instance Tag Type where tag = id
instance Tag Bind where 
    tag b = case b of 
        Var t i -> t
        FragCoord -> Type Float 4 1
        FragColor -> Type Float 4 1
instance Tag Tagged where tag (Tagged a) = tag a

-----------------------------------------------------------------------------
-- Matrices
data Mat a = Mat [[a]] deriving (Functor, Show)
instance Tag a => Tag (Mat a) where 
 tag (Mat xs) = let a = tag (head (head xs))
                    ls = map length xs
                    n = length ls
                    m = enforce ls
                    t = case a of
                        Type t 1 1 -> t
                        Type t n m -> error $ "Scalar expected, but got "
                                        ++ show n ++ "," ++ show m
                    enforce [x] = x
                    enforce (x:y:xs) = if x == y 
                        then enforce (y:xs)
                        else error "Inconsistent vector or matrix dimension"
                in if n > 4 || m > 4
                    then error "Maximum matrix dimension is 4"
                    else if n == 1 && m < 5
                        then Type t m 1 
                        else Type t n m
------------------------------------------------------------------------------
-- Type semantics
-- Expression language.
-- Supports literals, matrices of expressions,
-- multiplication- and addition-precedence operations,
-- comparison operations, unary and binary primitives,
-- value lookups by reference, function calls,
-- "swizzling" (selecting vector components, e.g. a.xy or a.yzx)
-- Currently functions are considered to all be unary.
-- In previous iterations of this,
-- a "currying" approach or heterogenous argument lists were used.
-- These approaches could be used as an extension to the current syntax,
-- though were not implemented here due to lack of time or pressing need.
-- If-expressions are the C-style "ternary operator",
-- which returns one of two cases depending on the predicate.
class Expr expr where
    lit     :: (Show a, Tag a) => a -> expr a
    matrix  :: Mat (expr a) -> expr (Mat a)
    mulOp   :: String -> expr a -> expr b -> expr c
    addOp   :: String -> expr a -> expr b -> expr c  
    compOp  :: String -> expr a -> expr a -> expr Bool
    prim    :: String -> expr a -> expr b
    prim2   :: String -> expr a -> expr b -> expr c
    val     :: Bind -> expr a
    call    :: Bind -> expr a -> expr b
    swiz    :: expr (Mat a) -> String -> expr b
    ifexpr  :: expr Bool -> expr a -> expr a -> expr a

-- Statements include setting a parameter (not a real GLSL feature,
-- as this happens in the function prototype)
-- (used to build the function - currently,
-- multiple parameters are not supported
-- (as noted above, a previous incarnation of this code
-- used multiple parameters, and accumulated them in an argument list -
-- but separate issues led me to simplify things to get a working result).
-- Set performs assignment to a mutable local variable, which is declare
-- (if it doesn't yet exist). The return type is a usable binding.
-- IfElse, For, and While as well as Break, Continue, and Return work like in C.
-- "Halt" is void return (with no returned expression).
-- Discard immediately halts, and discards the whole fragment being shaded.
-- NoOp does nothing (and isn't even printed).
class Stmt stmt where
    param   :: Type -> stmt Bind
    set     :: Bind -> TagE a -> stmt Bind
    ifElse  :: TagE Bool -> stmt () -> stmt () -> stmt ()
    for     :: TagE Int -> (TagE Int -> TagE Bool) -> (TagE Int -> TagE Int) 
            -> stmt () -> stmt ()
    while   :: TagE Bool -> stmt () -> stmt ()
    brk   :: stmt ()
    cont    :: stmt ()
    ret     :: TagE a -> stmt ()
    halt    :: stmt ()
    discard :: stmt ()
    noOp    :: stmt ()


--Declarations include uniforms
--(the input parameters to a fragment program),
--values (global declarations),
--processes (functions or procedures with local state),
--and the main process (a special case of processes,
--set aside for naming/typing simplicity,
--which takes no arguments and returns void.
class Decl decl where
    uni     :: Either Type (TagE a) -> decl Bind
    valu    :: TagE a -> decl Bind
    proc    :: WriteProc () -> decl (TagE a -> TagE b)
    fragMain:: WriteProc () -> decl ()

------------------------------------------------------------------------------
-- Tagged data structures
-- Analogous to the semantics presented above, but with types moved into tags.
data TagExpr = Lit Type Tagged
             | GMat Type (Mat TagExpr)
             | MulOp String Type TagExpr TagExpr
             | AddOp String Type TagExpr TagExpr
             | CompOp String TagExpr TagExpr
             | Prim String Type Type TagExpr
             | Prim2 String Type Type Type TagExpr TagExpr
             | Call Bind TagExpr
             | Val Bind
             | Swiz TagExpr Type String
             | IfExpr Type TagExpr TagExpr TagExpr
    
data TagStmt = Param Bind
             | DecVal Int Type TagExpr
             | Mutate Bind TagExpr
             | Block [TagStmt]
             | IfElse TagExpr TagStmt TagStmt
             | For Bind TagExpr TagExpr TagExpr TagStmt
             | While TagExpr TagStmt
             | Ret TagExpr
             | Discard
             | Halt
             | Break
             | Cont
             | NoOp

data TagDecl = Uni   Int Type (Maybe TagExpr)
             | Value Int Type TagExpr
             | Proc  Int Type [Bind] TagStmt 
             | Main TagStmt
------------------------------------------------------------------------------
-- Expr instance : TagE
-- TagE has the type semantics of Expr,
-- but moves all type information into tags
-- while building up the expression structure.
-- The underlying data is a TagExpr.
newtype TagE a = TagE {mkExpr :: TagExpr} deriving (Functor)
instance Tag (TagE a) where tag (TagE t) = tag t

-- TagExprs carry their type, so determining it is
-- accomplished through pattern matching against the precomputed type.
instance Tag TagExpr where
    tag t = case t of
        Lit t _             -> t
        MulOp _ t _ _       -> t
        AddOp _ t _ _       -> t
        CompOp _ _ _        -> Type Bool 1 1
        Prim _ t _ _        -> t
        Prim2 _ t _ _ _ _   -> t
        Val b               -> tag b
        Call f xs           -> tag f
        Swiz b t s          -> case t of Type r n 1 -> Type r (length s) 1
        GMat t _            -> t 
        IfExpr t _ _ _      -> t

-- Instances for the Expr implementation.
instance Expr TagE where
    lit a           = TagE $ Lit (tag a) (Tagged a)
    matrix xs       = TagE $ GMat (tag xs) (fmap mkExpr xs) 
    mulOp s x y     = TagE $ MulOp s (case s of 
        "*"         -> mulTag x y
        o           -> opTag x y) (mkExpr x) (mkExpr y) 
    addOp s x y     = TagE $ AddOp s (opTag x y) (mkExpr x) (mkExpr y)
    compOp s x y    = TagE $ CompOp s (mkExpr x) (mkExpr y)
    prim s x        = TagE $ Prim s (primTag s $ tag x) (tag x) (mkExpr x)
    prim2 s x y     = TagE $ Prim2 s (prim2Tag s (tag x) (tag y)) 
                        (tag x) (tag y) (mkExpr x) (mkExpr y)
    val b           = TagE $ Val b
    call f xs       = TagE $ Call f (mkExpr xs)
    swiz b s        = TagE $ Swiz (mkExpr b) t s
        where t     = case tag b of Type t n 1 -> Type t (length s) 1
    ifexpr p i e    = TagE $ IfExpr (tag i) (mkExpr p) (mkExpr i) (mkExpr e)

------------------------------------------------------------------------------

-- Statement typeclass instance
-- Writing a procedure uses an input environment,
-- and produces a list of statements.
-- Earlier, this was based on the RWS monad
-- (with the Decls in the reader and Stmts in the writer portion).
-- However, the environment, declarations, and counter
-- are all consistent throughout the process,
-- and State ended up being the simplest way to structure this.
type WriteProc = State (([TagDecl], [TagStmt]), Int)
runProc :: WriteProc a -> [TagDecl] -> Int -> (TagStmt, Int)
runProc w e i = let ((env,stmts), i') = execState w ((e,[]), i) 
                 in (Block stmts, i')

-- Monadic instance for Stmt
-- Update the list of output Stmts,
-- as well as the environment.
-- After using param and set,
-- bindings are returned that can be used
-- to refer to the involved values later.
-- The counter is used to provide unique variable names.
instance Stmt WriteProc where
    param t = do
        i <- nexti
        let b = Var t i
        tell [Param b]
        return b
    set b e = do
        d <- localenvf (search b)
        let x = mkExpr e
        case d of
            Just y -> tell [Mutate b x] >> return b
            Nothing -> case b of 
                FragCoord -> error "Cannot set gl_FragCoord"
                FragColor -> tell [Mutate FragColor x] >> return b
                (Var t i) -> do
                    localupd (extend (Value i t (mkExpr e)))
                    tell [DecVal i (tag e) x]
                    return b
    ifElse p i e = do
        u <- localenv
        n1 <- nexti
        let (ifCase, n2) = runProc i u n1
            (elseCase, n3) = runProc e u n2
        puti n3
        tell [IfElse (mkExpr p) ifCase elseCase]
    for s p t d = do
        i <- nexti
        i2 <- nexti
        u <- localenv
        let ib = Var (Type Int 1 1) i
            iv = val ib
            (act, i3) = runProc d u i2
        puti i3
        tell [For ib (mkExpr s) (mkExpr (p $ iv)) 
                    (mkExpr (t $ iv)) act]
    while e s = do 
        u <- localenv
        i <- nexti
        let (act, i2) = runProc s u i
        puti i2
        tell [While (mkExpr e) act]
    ret r = tell [Ret (mkExpr r)]
    brk = tell [Break]
    cont = tell [Cont]
    halt = tell [Halt]
    discard = tell [Discard]
    noOp = tell [NoOp]

------------------------------------------------------------------------------
-- Decl instance : WriteProg
--  Similar to Stmt, Decls are built up piecewise,
--  with a consistent environment state.
type WriteProg = State ([TagDecl], Int)
runProg :: WriteProg a -> [TagDecl]
runProg p = reverse $ fst $ execState p ([], 0)
instance Decl WriteProg where
    uni e = do
        i <- nexti
        let (t,x) = case e of
                Left t -> (t, Nothing)
                Right r -> (tag r, Just r)
            u = Uni i t (fmap mkExpr x)
        upd (extend u)
        return (Var t i)
    valu e = let t = tag e in do
        i <- nexti
        let v = Value i t (mkExpr e)
        upd (extend v) 
        return (Var t i)
    proc s = do
        i1 <- nexti
        e <- env
        let (st, i2) = runProc s e i1
            p = Proc i1 t ts st
            (t, ts) = tagStmt st
        puti i2
        upd (extend p) 
        return (call (Var t i1))
    fragMain s = do
        (e,i1) <- get
        let (st, i2) = runProc s e i1
            m = Main st
        puti i2
        upd (extend m)

------------------------------------------------------------------------------
-- Synonyms
bool_t = Type Bool 1 1
int_t = Type Int 1 1
float_t = Type Float 1 1
vec_t n = Type Float n 1
mat_t n m = Type Float n m

int :: Expr expr => Int -> expr Int 
int x = lit x

float :: Expr expr => Float -> expr Float
float x = lit x

mat :: Expr expr => [[expr Float]] -> expr (Mat Float)
mat = matrix . Mat

vec :: Expr expr => [expr Float] -> expr (Mat Float)
vec = matrix . Mat . (:[])

uarg :: (Functor decl, Decl decl) => Type -> decl (TagE a)
uarg = fmap val . uni . Left

udef :: (Functor decl, Decl decl) => TagE a -> decl (TagE a)
udef = fmap val . uni . Right

arg :: (Functor stmt, Stmt stmt) => Type -> stmt (TagE a)
arg = fmap val . param

value :: (Functor decl, Decl decl) => TagE a -> decl (TagE a)
value = fmap val . valu

bind :: WriteProc (TagE a) -> WriteProc Bind
bind m = do 
    e <- m 
    case mkExpr e of
        Val b -> return b
        x -> do
            i <- nexti
            set (Var (tag x) i) e

mkBinding :: Type -> WriteProc Bind
mkBinding t = do
    i <- nexti
    return (Var t i) 

mkFloat = mkBinding float_t
mkInt = mkBinding int_t
mkBool = mkBinding bool_t
mkVec = mkBinding . vec_t
mkMat n = mkBinding . mat_t n

fragCoord, fragColor :: Expr expr => expr (Mat Float)
fragCoord = val FragCoord
fragColor = val FragColor

setColor :: TagE (Mat Float) -> WriteProc ()
setColor m = set FragColor m >> return ()

------------------------------------------------------------------------------
-- Misc internal functions
-- Advance the internal counter state
nexti       :: MonadState (a,Int) m => m Int
nexti       = get >>= (\(_,i) -> puti (i+1) >> return (i + 1))
-- Set the internal counter state
puti        :: MonadState (a,Int) m => Int -> m ()
puti i      = get >>= (\(a,_) -> put (a,i))

-- Some primitives may (as special cases) return an unintuitive type.
-- However no such functions are defined here yet.
primTag :: String -> Type -> Type
primTag s t = t
prim2Tag :: String -> Type -> Type -> Type
prim2Tag s t u = t

-- Enforce dimension on math operations
-- Scalars can be used on matrices or vectors for any operation,
-- and vectors or matrices with the same dimension can be used together.
opTag :: TagE a -> TagE b -> Type
opTag x y = case (tag x, tag y) of
  (Type a n m, Type _ n' m')
    | n == n' && m == m' || n' == 1 && m' == 1 -> Type a n m
    | n == 1 && m == 1 -> Type a n' m'
    | otherwise -> error "Inconsistent vector or matrix dimension"

-- For multiplication, matrix multiplication is used
-- (requiring different dimension enforcement)
mulTag :: TagE a -> TagE b -> Type
mulTag x y = case (tag x, tag y) of
  (Type a n m, Type _ n' m')
    | n == 1 && m == 1 -> Type a n' m' 
    | m == 1 && m' == 1 && (n == n' || n' == 1) -> Type a n 1
    | n' == m -> Type a n m' 
    | otherwise -> error "Incorrect dimensions for matrix multiplication"

-- Find the type of a statement.
-- This is the return type if any exists,
-- or other wise void.
tagStmt :: TagStmt -> (Type, [Bind])
tagStmt s = case s of
    Param t -> (Void, [t])
    Ret a -> (tag a, [])
    IfElse _ i e -> unify (tagStmt i) (tagStmt e)
    For _ _ _ _ s -> tagStmt s
    While _ s -> tagStmt s
    Block s -> foldr1 unify (map tagStmt s)    
    otherwise -> (Void, [])
    
-- Combine like types, or combine anything with void
-- Fail with an error for conflicting types
unify :: (Type, [Bind]) -> (Type, [Bind]) -> (Type, [Bind])
unify (a, as) (b, bs)
    | a == b = (a, as ++ bs)
    | a == Void = (b, as ++ bs)
    | b == Void = (a, as ++ bs)
    | otherwise = 
        error "Conflicting return types in function definition"

extend :: TagDecl -> [TagDecl] -> [TagDecl]
extend = (:)

search ::  Bind -> [TagDecl] -> Maybe TagDecl
search b [] = Nothing
search (Var t i) (x:xs) = case x of
    Value i' t' e -> if i == i' && t == t'
        then Just (Value i t e)
        else search (Var t i) xs
    other -> search (Var t i) xs
search b xs = Nothing

mkDecl :: TagE a -> Int -> TagDecl
mkDecl a i = Value i (tag a) (mkExpr a)

-- Fake writer operation for outputting statements
tell :: [TagStmt] -> WriteProc ()
tell d = modify (\((e,s),i) -> ((e,s++d),i))

-- Fake reader monad for finding statements from the environment.
-- Unlike the real reader monad, this environment persists throughout
-- the lifetime of the context used to build the process
-- (and is not actually locally scoped - except in terms of this function).
localenvf :: ([TagDecl] -> a) -> WriteProc a
localenv = localenvf id

localenvf f = fmap (f . fst . fst) get

localupd :: ([TagDecl] -> [TagDecl]) -> WriteProc ()
localupd f = modify (first (first f))

-- Analogous function for decls
envf :: ([TagDecl] -> a) -> WriteProg a
envf f = fmap (f . fst) get
env = envf id

upd :: ([TagDecl] -> [TagDecl]) -> WriteProg ()
upd f = modify (first f)


