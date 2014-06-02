{-# Language DeriveFunctor, 
             FlexibleInstances,
             GADTs, 
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
    prim2   :: Tag a => String -> expr a -> expr b -> expr c
    call    :: Tag a => decl (a -> b) -> expr a -> expr b
    val     :: Tag a => Bind -> expr a
    swiz    :: Tag a => Bind -> String -> expr a

class Decl decl where
    uni     :: Either Type (expr a) -> decl (expr a)
    value   :: expr a -> decl (expr a)
    proc    :: stmt a -> decl a

class Stmt stmt where
    set     :: decl a -> stmt ()
    ifElse  :: expr Bool -> stmt a -> stmt a -> stmt a
    caseOf  :: [(Int, stmt a)] -> stmt a
    for     :: expr Int -> expr (Int -> Bool) -> expr (Int -> Int) -> stmt a -> stmt a
    while   :: expr Bool -> stmt a -> stmt a
    break   :: stmt ()
    cont    :: stmt ()
    ret     :: expr a -> stmt a 
    halt    :: stmt ()
    discard :: stmt ()
    noOp    :: stmt () -> stmt next

------------------------------------------------------------------------------
-- Tagged type-erased data structures
data TagExpr = Lit Type Erased
             | MulOp String Type TagExpr TagExpr
             | AddOp String Type TagExpr TagExpr
             | CompOp String Type TagExpr TagExpr
             | Prim String Type Type TagExpr
             | Prim2 String Type Type Type TagExpr TagExpr
             | Call Type [Type]
             | Val Bind
             | Swiz Bind String
             deriving Show

data TagDecl = Uni   String Type (Maybe TagExpr)
             | Value String Type TagExpr
             | Proc  String Type [Type] TagStmt 

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

newtype TagE a = TagE {erase :: TagExpr} deriving (Functor, Show)

------------------------------------------------------------------------------
-- Type erasure and tagging
data PrimType = Bool | Int | Float deriving (Eq, Show)
data Type = Type PrimType Int Int deriving (Eq, Show)
data Bind = Var Type Int deriving (Eq, Show)
data Erased = forall a . Show a => Erased a
deriving instance Show Erased

class Tag a where tag :: a -> Type

instance Tag Int where tag = const (Type Int 1 1)
instance Tag Bool where tag = const (Type Bool 1 1)
instance Tag Float where tag = const (Type Float 1 1)
instance Tag a => Tag [a] where tag xs = let Type t m 1 = sameDim $ map tag xs
                                         in Type t (length xs) m

------------------------------------------------------------------------------
instance Tag a => Tag (TagE a) where
    tag (TagE t) = case t of
        Lit t _ -> t
        MulOp _ t _ _ -> t
        -- TODO MISSING INSTANCES

instance Expr TagE where
    lit a       = TagE $ Lit (tag a) (Erased a)
    mulOp s x y = TagE $ MulOp s (tag x) (erase x) (erase y) 
    -- TODO MISSING INSTNACES

------------------------------------------------------------------------------
type WriteDecl = StateT Int (Writer [TagDecl])
instance Decl WriteDecl
type WriteProc = RWS [TagDecl] [TagStmt] Int
instance Stmt WriteProc
------------------------------------------------------------------------------
-- Misc functions

nexti       :: WriteDecl Int
nexti       = get >>= \i -> put (i + 1) >> return i

sameDim :: (Eq a, Show a) => [a] -> a
sameDim [x] = x
sameDim (x:y:xs) = if x == y 
    then sameDim (y:xs) 
    else error $ concat ["Inconsistent vector or matrix dimension (",
                          show x, ") /= (", show y, ")"]

