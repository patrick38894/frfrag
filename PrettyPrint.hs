{-# Language DeriveFunctor, StandaloneDeriving,
             FlexibleInstances,
             GADTs,
             IncoherentInstances,
             PolyKinds,
             RankNTypes,
             UndecidableInstances #-}

module Fragment where
import Control.Monad(guard)
import Data.Maybe(isJust)
import Text.PrettyPrint.HughesPJ
import Control.Monad.State
------------------------------------------------------------------------------
-- Pretty printing -----------------------------------------------------------

-- Fragment source files are printed by concatenating the sections.
instance PP FragSrc where
    pp (Frag us os ds rs mn) = 
        vcat [pp us, pp os, pp ds, ppmain (shadeRegion (region rs) mn)]

-- Show instances based on pp
instance Show FragSrc where show = render . pp
instance Show Eval where show = show . eval
instance Show (Binding a) where show = render . pp 
instance PP a => Show (VecN a) where show = render . pp
instance PP a => Show (MatN a) where show = render . pp
instance PP a => Show (Rep a) where show = render . pp
instance (GetRep a, PP a) => Show (Expr a) where show = render . pp
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
instance PP (HetList f) where pp = pplist . pphlist

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
        -- BinOp nm e1 e2 -> pp e1 <+> pp nm <+> pp e2

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
                               <+> pp (test \$ Val ix) <> semi 
                               <+> pp (Mutate ix (iter \$ Val ix)))
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
pphlist (HCons b bs) = 
    (case b ~=~ PPClass of Just Refl -> pp b; Nothing -> error "can't pp") : pphlist bs

data PPClass :: * -> * where
    PPClass :: (PP a) => a -> PPClass a

braceBlock :: Doc -> Doc
braceBlock d = lbrace $+$ nest 4 d $+$ rbrace

ppcases :: CaseList -> Doc
ppcases = vcat . punctuate semi . pphlist

semiBind :: ArgList -> Doc
semiBind = vcat . punctuate semi . pphlist

ppmain :: Stmt Void -> Doc
ppmain mn =  pp "void main" <> parens empty 
             $+$ braceBlock (pp mn)


------------------------------------------------------------------------------
-- Other instances that glue this all together -------------------------------

-- Type equality witness
data Equal :: k -> k -> * where Refl :: Equal x x
-- General test for equality of types inside of a constructor f
-- Used to compare types inside of a Rep a or a Binding a.
class RepF f where (~=~) :: f a -> f b -> Maybe(Equal a b)

-- Eq instance for HetLists with the same f.
-- Since every (f a) in a HetList satisfies Eq,
-- they can be checked against each other one by one -
--  as soon as a ~=~ c proves that the two element types are the same.
instance Eq (HetList f) where
    HNil == HNil = True
    HCons a bs == HCons c ds = case a ~=~ c of Just Refl -> a == c && bs == ds
                                               Nothing -> False

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

-- Bindings can be checked for equality by comparing the name, and all types involved.
instance RepF Binding where
    Var s1 t1 ~=~ Var s2 t2 = guard (s1 == s2) >> (t1 ~=~ t2) 
    Proc s1 t1 as1 ~=~ Proc s2 t2 as2 = do guard (s1 == s2 && as1 == as2)
                                           Refl <- (t1 ~=~ t2)
                                           Just Refl     
    Rec s1 as1 ~=~ Rec s2 as2 = guard (s1 == s2 && as1 == as2) >> Just Refl
    a ~=~ b = Nothing

-- Equality for statements and their types
instance RepF Stmt where
    Block l1 ~=~ Block l2 = if l1 == l2 then Just Refl else Nothing

instance Eq (Stmt a) where
    Block l1 == Block l2 = l1 == l2
