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
data PrimType = Bool | Int | Float deriving Eq
data Type = Type PrimType Int Int | Void deriving Eq
data Bind = Var Type Int | FragCoord | FragColor deriving Eq
data Tagged = forall a . (Show a, Tag a) => Tagged a
data TaggedF expr = forall a . TaggedF (expr a)

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
 tag (Mat xs) = let (Type t 1 1) = tag (head (head xs))
                    ls = map length xs
                    n = length ls
                    m = enforce ls
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

class Abst abst where
    lam     :: Bind -> Bind -> abst (a -> b)
    app     :: abst (a -> b) -> abst a -> abst b

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

class Decl decl where
    uni     :: Either Type (TagE a) -> decl Bind
    valu    :: TagE a -> decl Bind
    proc    :: WriteProc () -> decl (TagE a -> TagE b)
    fragMain:: WriteProc () -> decl ()

------------------------------------------------------------------------------
-- Tagged data structures
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

newtype TagE a = TagE {mkExpr :: TagExpr} deriving (Functor)
instance Tag (TagE a) where tag (TagE t) = tag t

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
        Swiz b t s          -> t
        GMat t _            -> t 
        IfExpr t _ _ _      -> t

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

type WriteProc = State (([TagDecl], [TagStmt]), Int)
runProc :: WriteProc a -> [TagDecl] -> Int -> (TagStmt, Int)
runProc w e i = let ((env,stmts), i) = execState w ((e,[]), i) 
                 in (Block stmts, i)


tell :: [TagStmt] -> WriteProc ()
tell d = modify (\((e,s),i) -> ((e,s++d),i))

localenvf :: ([TagDecl] -> a) -> WriteProc a
localenv = localenvf id

localenvf f = fmap (f . fst . fst) get

localupd :: ([TagDecl] -> [TagDecl]) -> WriteProc ()
localupd f = modify (first (first f))

envf :: ([TagDecl] -> a) -> WriteProg a
envf f = fmap (f . fst) get
env = envf id

upd :: ([TagDecl] -> [TagDecl]) -> WriteProg ()
upd f = modify (first f)

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
                FragColor -> tell [Mutate FragColor x] >> return b
                other -> do
                    i <- nexti
                    let d' = mkDecl e i
                    localupd (extend d')
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

type WriteProg = State ([TagDecl], Int)
runProg :: WriteProg a -> [TagDecl]
runProg p = fst $ execState p ([], 0)
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
        i1 <- nexti
        e <- env
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


setColor :: TagE (Mat Float) -> WriteProc ()
setColor m = set FragColor m >> return ()

------------------------------------------------------------------------------
-- Misc internal functions
nexti       :: MonadState (a,Int) m => m Int
nexti       = get >>= (\(_,i) -> puti (i+1) >> return (i + 1))

puti        :: MonadState (a,Int) m => Int -> m ()
puti i      = get >>= (\(a,_) -> put (a,i))

primTag :: String -> Type -> Type
primTag s t = t -- TODO check if this is actually right
prim2Tag :: String -> Type -> Type -> Type
prim2Tag s t u = t -- TODO check if this is actually right

opTag :: TagE a -> TagE b -> Type
opTag x y = case (tag x, tag y) of
  (Type a n m, Type _ n' m')
    | n == n' && m == m' || n' == 1 && m' == 1 -> Type a n m
    | n == 1 && m == 1 -> Type a n' m'
    | otherwise -> error "Inconsistent vector or matrix dimension"

mulTag :: TagE a -> TagE b -> Type
mulTag x y = case (tag x, tag y) of
  (Type a n m, Type _ n' m')
    | n == 1 && m == 1 -> Type a n' m' 
    | m == 1 && m' == 1 && (n == n' || n' == 1) -> Type a n 1
    | n' == m -> Type a n m' 
    | otherwise -> error "Incorrect dimensions for matrix multiplication"

tagStmt :: TagStmt -> (Type, [Bind])
tagStmt s = case s of
    Param t -> (Void, [t])
    Ret a -> (tag a, [])
    IfElse _ i e -> unify (tagStmt i) (tagStmt e)
    For _ _ _ _ s -> tagStmt s
    While _ s -> tagStmt s
    Block s -> foldr1 unify (map tagStmt s)    
    otherwise -> (Void, [])
    
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

