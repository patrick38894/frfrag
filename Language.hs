{-# Language DeriveFunctor, 
             FlexibleContexts,
             FlexibleInstances,
             GADTs,
             IncoherentInstances,
             MultiParamTypeClasses, 
             RankNTypes,
             StandaloneDeriving #-}

module Language where
import Control.Monad.RWS

------------------------------------------------------------------------------
-- Type representations
data PrimType = Bool | Int | Float deriving (Eq, Show)
data Type = Type PrimType Int Int | Void deriving (Eq, Show)
data Bind = Var Type Int deriving (Eq, Show)
data Erased = forall a . (Show a, Tag a) => Erased a; deriving instance Show Erased

class Tag a where tag :: a -> Type
instance Tag Bool where tag = const (Type Bool 1 1)
instance Tag Int where tag = const (Type Int 1 1)
instance Tag Float where tag = const (Type Float 1 1)
instance Tag Type where tag = id
instance Tag Bind where tag (Var t i) = t
instance Tag Erased where tag (Erased a) = tag a

------------------------------------------------------------------------------
-- Matrices
data Mat a = Mat [[a]] deriving Show
instance Tag a => Tag (Mat a) where 
 tag (Mat xs) = let (Type t 1 1) = tag (head (head xs))
                    ls = map length xs
                    n = length ls
                    m = enforce ls
                    enforce [x] = x
                    enforce (x:y:xs) = if x == y 
                        then enforce (y:xs)
                        else error "Inconsistent vector or matrix dimension"
                in if n == 1 then Type t m 1 else Type t n m
------------------------------------------------------------------------------
-- Type semantics
class Expr expr where
    lit     :: (Show a, Tag a) => a -> expr a
    mulOp   :: Tag a => String -> expr a -> expr a -> expr a
    addOp   :: Tag a => String -> expr a -> expr a -> expr a    
    compOp  :: Tag a => String -> expr a -> expr a -> expr Bool
    prim    :: Tag a => String -> expr a -> expr b
    prim2   :: (Tag a, Tag b) => String -> expr a -> expr b -> expr c

class Refr refr where
    val     :: Tag a => Bind -> refr a
    call    :: Tag a => Bind -> [Bind] -> refr a
    swiz    :: Tag a => Bind -> String -> refr a

class Abst abst where
    lam     :: Bind -> Bind -> abst (a -> b)
    app     :: abst (a -> b) -> abst a -> abst b

class Stmt stmt where
    param   :: Tag a => Type -> stmt Bind
    set     :: Tag a => Bind -> TagE a -> stmt Bind
    ifElse  :: TagE Bool -> stmt () -> stmt () -> stmt ()
    for     :: TagE Int -> (TagE Int -> TagE Bool) -> (TagE Int -> TagE Int) 
            -> stmt () -> stmt ()
    while   :: TagE Bool -> stmt () -> stmt ()
    break   :: stmt ()
    cont    :: stmt ()
    ret     :: Tag a => TagE a -> stmt ()
    halt    :: stmt ()
    discard :: stmt ()
    noOp    :: stmt ()

class Decl decl where
    uni     :: Tag a => Either Type (TagE a) -> decl (TagE a)
    value   :: Tag a => TagE a -> decl (TagE a)
    proc    :: Tag a => WriteProc () -> decl ([Bind] -> TagE a)


------------------------------------------------------------------------------
-- Tagged data structures
data TagExpr = Lit Type Erased
             | MulOp String Type TagExpr TagExpr
             | AddOp String Type TagExpr TagExpr
             | CompOp String Type TagExpr TagExpr
             | Prim String Type Type TagExpr
             | Prim2 String Type Type Type TagExpr TagExpr
             | Call Bind [Bind]
             | Val Bind
             | Swiz Bind Type String
             deriving Show

data TagStmt = Param Bind
             | DecVal TagDecl
             | Mutate TagDecl
             | Block [TagStmt]
             | IfElse TagExpr TagStmt TagStmt
             | For TagDecl TagExpr TagExpr TagStmt
             | While TagExpr TagStmt
             | Ret TagExpr
             | Discard
             | Halt
             | Break
             | Cont
             | NoOp
             deriving Show

data TagDecl = Uni   Int Type (Maybe TagExpr)
             | Value Int Type TagExpr
             | Proc  Int Type [Type] TagStmt 
             deriving Show
------------------------------------------------------------------------------
-- Expr instance : TagE

newtype TagE a = TagE {mkExpr :: TagExpr} deriving (Functor, Show)
instance Tag a => Tag (TagE a) where tag (TagE t) = tag t

instance Tag TagExpr where
    tag t = case t of
        Lit t _             -> t
        MulOp _ t _ _       -> t
        AddOp _ t _ _       -> t
        CompOp _ t _ _      -> t
        Prim _ t _ _        -> t
        Prim2 _ t _ _ _ _   -> t
        Val b               -> tag b
        Call f xs           -> tag f
        Swiz b t s          -> t

instance Expr TagE where
    lit a           = TagE $ Lit (tag a) (Erased a)
    mulOp s x y     = TagE $ MulOp s (case s of 
        "*"         -> mulTag x y
        o           -> opTag x y) (mkExpr x) (mkExpr y) 
    addOp s x y     = TagE $ AddOp s (opTag x y) (mkExpr x) (mkExpr y)
    compOp s x y    = TagE $ CompOp s (opTag x y) (mkExpr x) (mkExpr y)
    prim s x        = TagE $ Prim s (primTag s $ tag x) (tag x) (mkExpr x)
    prim2 s x y     = TagE $ Prim2 s (prim2Tag s (tag x) (tag y)) 
                        (tag x) (tag y) (mkExpr x) (mkExpr y)
    
instance Refr TagE where
    val b = TagE $ Val b
    call f xs  = TagE $ Call f xs
    swiz b s = TagE $ Swiz b t s
        where t = case b of Var (Type t n 1) i -> Type t (length s) 1

------------------------------------------------------------------------------

type WriteProc = RWS [TagDecl] [TagStmt] Int
runProc :: WriteProc () -> [TagDecl] -> Int -> TagStmt
runProc w e i = Block $ snd $ evalRWS w e i
instance Show (WriteProc ()) where show w = show $ runProc w [] 0

instance Stmt WriteProc where
    param t = do
        i <- nexti
        let b = Var t i
        tell [Param b]
        return b
    set b e = do
        i <- nexti
        d <- asks (search b)
        let d' = mkDecl e i
        case d of
            Just x -> tell [Mutate d']
            Nothing -> local (extend d') $ tell [DecVal d']
        return b
    ifElse p i e = do
        u <- ask
        n <- get
        tell [IfElse (mkExpr p) (runProc i u n) (runProc e u n)]
    for s p t d = do
        i <- nexti
        u <- ask
        let v = mkDecl s i
        tell [For v (mkExpr (p $ val (Var (Type Int 1 1) i))) 
                    (mkExpr (t $ val (Var (Type Int 1 1) i))) (runProc d u i)]
    while e s = do 
        u <- ask
        i <- get
        tell [While (mkExpr e) (runProc s u i)]
    ret r = tell [Ret (mkExpr r)]
    break = tell [Break]
    cont = tell [Cont]
    halt = tell [Halt]
    discard = tell [Discard]
    noOp = tell [NoOp]

------------------------------------------------------------------------------
-- Decl instance : WriteProg

type WriteProg = RWS [TagDecl] [TagDecl] Int
runProg :: WriteProg a -> [TagDecl]
runProg p = snd $ evalRWS p [] 0
instance Show (WriteProg a) where show = show . runProg
instance Decl WriteProg where
    uni e = do
        i <- nexti
        let (t,x) = case e of
                Left t -> (t, Nothing)
                Right r -> (tag r, Just r)
            u = Uni i t (fmap mkExpr x)
        local (extend u) $ tell [u]
        return (val (Var t i))
    value e = let t = tag e in do
        i <- nexti
        let v = Value i t (mkExpr e)
        local (extend v) $ tell [v]
        return (val (Var t i))
    proc s = do
        i <- nexti
        e <- ask
        let st = runProc s e i
            (t, ts) = tagStmt st
            p = Proc i t ts st
        local (extend p) $ tell [p]
        return (call (Var t i)) 

------------------------------------------------------------------------------
-- Misc functions

nexti       :: MonadState Int m => m Int
nexti       = get >>= \i -> put (i + 1) >> return i

primTag :: String -> Type -> Type
primTag s t = t -- TODO check if this is actually right
prim2Tag :: String -> Type -> Type -> Type
prim2Tag s t u = t -- TODO check if this is actually right

opTag :: Tag a => TagE a -> TagE a -> Type
opTag x y = case (tag x, tag y) of
  (Type a n m, Type _ n' m') 
    | n == n' && m == m' || n' == 1 && m' == 1 -> Type a n m
    | n == 1 && m == 1 -> Type a n' m'
    | otherwise -> error "Inconsistent vector or matrix dimension"

mulTag :: Tag a => TagE a -> TagE a -> Type
mulTag x y = case (tag x, tag y) of
  (Type a n m, Type _ m' o) -- -> case m of
    | n == 1 && m == 1 -> Type a m' o
    | m' == 1 && (m == o || o == 1) -> Type a n 1
    | m' == m -> Type a n o 
    | otherwise -> error "Incorrect vector mult. dimension"

tagStmt :: TagStmt -> (Type, [Type])
tagStmt s = case s of
    Ret a -> (tag a, [])
    IfElse _ i e -> unify (tagStmt i) (tagStmt e)
    For _ _ _ s -> tagStmt s
    While _ s -> tagStmt s
    otherwise -> (Void, [])
    
unify :: (Type, [Type]) -> (Type, [Type]) -> (Type, [Type])
unify (a, as) (b, bs) = if a == b
    then (a, as ++ bs)
    else error "Conflicting return types in function definition"

extend :: TagDecl -> [TagDecl] -> [TagDecl]
extend = (:)

search ::  Bind -> [TagDecl] -> Maybe TagDecl
search b [] = Nothing
search (Var t i) (x:xs) = case x of
    Value i t e -> if i == i && t == t 
        then Just (Value i t e)
        else search (Var t i) xs
    other -> search (Var t i) xs

mkDecl :: Tag a => TagE a -> Int -> TagDecl
mkDecl a i = Value i (tag a) (mkExpr a)
