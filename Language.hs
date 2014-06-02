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
import Control.Monad.Writer
import Control.Monad.RWS

------------------------------------------------------------------------------
-- Core Language as typeclasses
class Expr expr where
    lit     :: (Show a, Tag a) => a -> expr a
    mulOp   :: Tag a => String -> expr a -> expr a -> expr a
    addOp   :: Tag a => String -> expr a -> expr a -> expr a    
    compOp  :: Tag a => String -> expr a -> expr a -> expr Bool
    prim    :: Tag a => String -> expr a -> expr b
    prim2   :: (Tag a, Tag b) => String -> expr a -> expr b -> expr c

class RunExpr expr rexp where
    run     :: Expr expr => expr a -> rexp

class Refr refr where
    val     :: Tag a => Bind -> refr a
    call    :: Tag a => Bind -> [Bind] -> refr a
    swiz    :: Tag a => Bind -> String -> refr a

class Abst abst where
    lam     :: Bind -> Bind -> abst (a -> b)
    app     :: abst (a -> b) -> abst a -> abst b

class Decl decl where
    uni     :: (Tag a, Refr refr) => Either Type (TagE a) -> decl (refr a)
    value   :: Tag a => TagE a -> decl (TagE a)
    proc    :: (Tag a, Stmt stmt) => stmt a -> decl ([Bind] -> TagE a)

class Stmt stmt where
    set     :: decl a -> stmt ()
    ifElse  :: expr Bool -> stmt a -> stmt a -> stmt a
    caseOf  :: [(Int, stmt a)] -> stmt a
    for     :: expr Int -> expr (Int -> Bool) -> expr (Int -> Int) 
            -> stmt a -> stmt a
    while   :: expr Bool -> stmt a -> stmt a
    break   :: stmt ()
    cont    :: stmt ()
    ret     :: expr a -> stmt a 
    halt    :: stmt ()
    discard :: stmt ()
    noOp    :: stmt ()

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
             | Swiz Bind String
             deriving Show

data TagDecl = Uni   Int Type (Maybe TagExpr)
             | Value Int Type TagExpr
             | Proc  Int Type [Type] TagStmt 

data TagStmt = DecVal TagDecl
             | Mutate TagDecl
             | IfElse TagExpr TagStmt TagStmt
             | CaseOf [(Int, TagStmt)]
             | For TagDecl TagExpr TagStmt
             | While TagExpr TagStmt
             | Ret TagExpr
             | Discard
             | Halt
             | Break
             | Cont
             | NoOp

newtype TagE a = TagE {mkTag :: TagExpr} deriving (Functor, Show)
------------------------------------------------------------------------------
-- Type erasure and tagging
data PrimType = Bool | Int | Float deriving (Eq, Show)
data Type = Type PrimType Int Int deriving (Eq, Show)
data Bind = Var Type Int deriving (Eq, Show)
data Erased = forall a . Show a => Erased a
data Vec a = Vec [a] deriving Show
type Mat a = Vec (Vec a)
deriving instance Show Erased

class Tag a where tag :: a -> Type

instance Tag Int where tag = const (Type Int 1 1)
instance Tag Bool where tag = const (Type Bool 1 1)
instance Tag Float where tag = const (Type Float 1 1)
instance Tag a => Tag (Vec a) where tag (Vec xs) = let Type t m 1 = sameDim $ map tag xs
                                                   in Type t (length xs) m

instance (Expr expr, Tag a) => Tag (expr a) where
    tag e = error "Default instance for Tag (expr a) not actually implemented"

------------------------------------------------------------------------------
instance Tag a => Tag (TagE a) where
    tag (TagE t) = case t of
        Lit t _ -> t
        MulOp _ t _ _ -> t
        AddOp _ t _ _ -> t
        CompOp _ t _ _ -> t
    -- TODO MISSING INSTANCES

instance Expr TagE where
    lit a       = TagE $ Lit (tag a) (Erased a)
    mulOp s x y = TagE $ MulOp s (case s of "*" -> mulTag x y
                                            o -> opTag x y) (mkTag x) (mkTag y) 
    addOp s x y = TagE $ AddOp s (opTag x y) (mkTag x) (mkTag y)
    compOp s x y = TagE $ CompOp s (opTag x y) (mkTag x) (mkTag y)
    prim s x = TagE $ Prim s (primTag s $ tag x) (tag x) (mkTag x)
    prim2 s x y = TagE $ Prim2 s (prim2Tag s (tag x) (tag y)) 
                                 (tag x) (tag y) (mkTag x) (mkTag y)
    
instance Refr TagE where
    val b = TagE $ Val b
    call f xs  = undefined -- TODO
    swiz b s = undefined -- TODO

------------------------------------------------------------------------------
type WriteProg = StateT Int (Writer [TagDecl])
instance Decl WriteProg where
    uni e = do
        i <- nexti
        let (t,x) = case e of
                    Left t -> (t, Nothing)
                    Right r -> (tag r, Just r)
        tell [Uni i t (fmap mkTag x)]
        return (val (Var t i))
    value e = let t = tag e in do
        i <- nexti
        tell [Value i t (mkTag e)]
        return (val (Var t i))
    proc s = do
        i <- nexti
        (t, ts) <- tagStmt s
        tell [Proc i t ts (mkStmt s)]
        return (call (Var t i)) 


tagStmt :: (Tag a, Stmt stmt) => stmt a -> WriteProg (Type, [Type])
tagStmt = undefined

mkArgs :: [Type] -> [Bind]
mkArgs = undefined
------------------------------------------------------------------------------
type WriteProc = RWS [TagDecl] [TagStmt] Int
instance Stmt WriteProc

mkStmt :: Stmt stmt => stmt a -> TagStmt
mkStmt = undefined

------------------------------------------------------------------------------
-- Misc functions

nexti       :: MonadState Int m => m Int
nexti       = get >>= \i -> put (i + 1) >> return i

sameDim :: (Eq a, Show a) => [a] -> a
sameDim [x] = x
sameDim (x:y:xs) = if x == y 
    then sameDim (y:xs) 
    else error $ concat ["Inconsistent vector or matrix dimension (",
                          show x, ") /= (", show y, ")"]
primTag :: String -> Type -> Type
primTag = undefined

prim2Tag :: String -> Type -> Type -> Type
prim2Tag = undefined

-- For ALL ops other than matrix multiplication
-- the dimension of the two exprs must be the same.
opTag :: Expr expr => expr a -> expr a -> Type
opTag = undefined

-- For multiplication, matrices need to be nxm x mxo = nxo
mulTag :: Expr expr => expr a -> expr a -> Type
mulTag = undefined


