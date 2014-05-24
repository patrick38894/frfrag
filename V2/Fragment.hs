{-# Language DataKinds,
             ExistentialQuantification, 
             FlexibleInstances,
             GADTs,
             KindSignatures,
             PolyKinds,
             RankNTypes,
             StandaloneDeriving,
             TypeSynonymInstances #-}
module V2.Fragment where
import Control.Monad
import Data.Char
import Data.Maybe
import GHC.Exts
import Text.PrettyPrint.HughesPJ


------------------------------------------------------------------------------
-- Structured GLSL program representation ------------------------------------

data FragSrc = Frag
                {
                    uniform :: HetList Binding,
                    decls :: HetList Decl,
                    main :: Stmt
                }

------------------------------------------------------------------------------
-- GLSL language representation ----------------------------------------------

data N = N2 | N3 | N4 deriving (Eq, Ord)
class AsInt n where asInt :: n -> Int
instance AsInt N where asInt n = case n of N2 -> 2; N3 -> 3; N4 -> 4

data VecN a where 
    Vec2 :: a -> a -> VecN a
    Vec3 :: a -> a -> a -> VecN a
    Vec4 :: a -> a -> a -> a -> VecN a

data MatN t = MatN (VecN (VecN t))

data Equal :: k -> k -> * where Refl :: Equal x x
class RepF f where (~=~) :: f a -> f b -> Maybe(Equal a b)

data HetList f where
    HNil :: HetList f
    HCons :: (RepF f, PP (f a), Eq (f a)) => f a -> HetList f -> HetList f

instance Eq (HetList f) where
    HNil == HNil = True
    HCons a bs == HCons c ds = isJust (a ~=~ c) && bs == ds

type ArgList = HetList Binding
type Args = HetList Expr

data Rep :: * -> * where
    BoolT :: Rep Bool
    IntT :: Rep Int
    FloaT :: Rep Float
    VecT :: Rep t -> N -> Rep (VecN t)
    MatT :: Rep t -> N -> N -> Rep (MatN t)
    StrucT :: String -> ArgList -> Rep String

class GetRep t where getRep :: t -> Rep t
instance GetRep Int where getRep = const IntT
instance GetRep Bool where getRep = const BoolT
instance GetRep Float where getRep = const FloaT
instance GetRep a => GetRep (VecN a) where
    getRep (Vec2 x y) = VecT (getRep x) N2
    getRep (Vec3 x y z) = VecT (getRep x) N3
    getRep (Vec4 x y z w) = VecT (getRep x) N4
instance GetRep a => GetRep (MatN a) where
    getRep (MatN vs) = case getRep vs of VecT vt n -> case vt of VecT t m -> MatT t n m 

instance RepF Rep where
    BoolT ~=~ BoolT = Just Refl
    IntT ~=~ IntT = Just Refl
    FloaT ~=~ FloaT = Just Refl
    VecT r1 n1 ~=~ VecT r2 n2 = do guard (n1 == n2); Refl <- (r1 ~=~ r2); Just Refl
    MatT r1 n1 m1 ~=~ MatT r2 n2 m2 = do guard (n1 == n2 && m1 == m2)
                                         Refl <- (r1 ~=~ r2)
                                         Just Refl
    StrucT nm1 as1 ~=~ StrucT nm2 as2 = do guard (nm1 == nm2 && as1 == as2); Just Refl
    r ~=~ s = Nothing

instance Eq (Rep a) where
    r == s = isJust (r ~=~ s)

data Binding :: * -> * where
    Val :: String -> Rep t -> Binding t
    Proc :: String -> Rep t -> ArgList -> Binding (ArgList -> t)
    Rec :: String -> ArgList -> Binding (ArgList -> String) 
    Dot :: Binding (ArgList -> String) -> String -> Binding a
    Swiz :: Binding (VecN a) -> String -> Binding b

instance RepF Binding where
    Val s1 t1 ~=~ Val s2 t2 = guard (s1 == s2) >> (t1 ~=~ t2) 
    Proc s1 t1 as1 ~=~ Proc s2 t2 as2 = do guard (s1 == s2 && as1 == as2)
                                           Refl <- (t1 ~=~ t2)
                                           Just Refl     
    Rec s1 as1 ~=~ Rec s2 as2 = guard (s1 == s2 && as1 == as2) >> Just Refl
    a ~=~ b = Nothing

data Expr :: * -> * where
    Float :: Float -> Expr Float
    Bool :: Bool -> Expr Bool
    Int :: Int -> Expr Int
    Vec :: VecN t -> Expr (VecN t)
    Mat :: MatN t -> Expr (MatN t)
    Var :: Binding t -> Expr t
    Call :: Binding ret -> Args -> Expr ret
    Con :: Binding rec -> Args -> Expr rec

data Decl :: * -> * where
    Value :: Binding t -> Expr t -> Decl t
    Process :: Binding (ArgList -> t) -> Stmt -> Decl (ArgList -> t)
    Struct :: Binding (ArgList -> String) -> Decl (ArgList -> String)

data Stmt where
    NoOp :: Stmt
    Block :: [Stmt] -> Stmt
    Mutate :: Binding t -> Expr t -> Stmt
    Extract :: Binding r -> Binding r -> Args -> Stmt 
    IfElse :: Expr Bool -> Stmt -> Stmt -> Stmt
    For :: Binding i -> Expr i -> Expr (i -> Bool) -> Expr (i -> i) -> Stmt -> Stmt
    While :: Expr Bool -> Stmt -> Stmt
    Break :: Stmt
    Continue :: Stmt
    Return :: Expr a -> Stmt
    Terminate :: Stmt
    Discard :: Stmt

------------------------------------------------------------------------------
-- Auxiliary stuff -----------------------------------------------------------

vecToList :: VecN t -> [t]
vecToList v = case v of
    Vec2 a b -> [a, b]
    Vec3 a b c -> [a, b, c]
    Vec4 a b c d -> [a, b, c, d]

vec2  = VecT FloaT N2 
vec3  = VecT FloaT N3 
vec4  = VecT FloaT N4 
ivec2 = VecT IntT  N2 
ivec3 = VecT IntT  N3 
ivec4 = VecT IntT  N4 
bvec2 = VecT BoolT N2 
bvec3 = VecT BoolT N3 
bvec4 = VecT BoolT N4 

------------------------------------------------------------------------------
-- Pretty printing -----------------------------------------------------------
instance Show N where show = render . pp
instance PP a => Show (VecN a) where show = render . pp
instance PP a => Show (MatN a) where show = render . pp
instance Show (HetList f) where show = render . pp 
instance Show (Rep a) where show = render . pp

class PP x where pp :: x -> Doc
instance PP Doc where pp = id
instance PP String where pp = text
instance PP Int where pp = text . show
instance PP N where pp = pp . asInt
instance PP a => PP (VecN a) where pp = sep . punctuate comma . map pp . vecToList
instance PP a => PP (MatN a) where 
    pp (MatN m) = pplist . concatMap vecToList . vecToList $ m
instance PP (HetList f) where
    pp = pplist . pphlist

instance PP (Rep a) where
    pp r = case r of
        BoolT        -> pp "bool"
        FloaT       -> pp "float"
        IntT         -> pp "int"
        VecT r n     -> getInitial r <> pp "vec" <> pp n
        MatT r n m   -> getInitial r <> pp "mat" <> pp n 
                        <> if m == n then empty else pp "x" <> pp m
        StrucT nm fs  -> pp nm 

instance (GetRep a, PP a) => PP (Expr a) where
    pp expr = case expr of
        Float n    -> pp (show n)
        Bool b     -> pp (if b then "true" else "false")
        Int n      -> pp (show n)
        Vec v      -> getInitial (getRep v) <> (parens $ pp v)
        Mat m      -> getInitial (getRep m) <> (parens $ pp m)
        Var (Val s r) -> pp s
        Call (Proc s r a) p -> pp s <> (parens (pp p))

instance PP (Binding t) where
    pp (Val s r) = pp r <+> pp s
    pp (Proc s r as) = pp r <+> pp s <> parens (pp as)
    pp (Rec s as) = pp s
    pp (Dot rec s) = pp rec <> text "." <> pp s
    pp (Swiz vec s) = pp vec <> text "." <> pp s

instance (GetRep t, PP t) => PP (Decl t) where
    pp dec = case dec of
        Value (Val s r) e -> pp r <+> pp s <+> equals <+> pp e
        Process (Proc s r a) b -> pp r <+> pp s <+> parens (pp a) 
                                 $+$ braceBlock (pp b)
        Struct (Rec s a) -> pp "struct" <+> pp s $+$ braceBlock (semiBind a)

instance PP Stmt where
    pp stmt = case stmt of
        NoOp -> empty
        Block stmts -> braceBlock (vcat (map pp stmts))

getInitial :: Rep a -> Doc
getInitial a = text $ case a of BoolT -> "b"; IntT -> "i"; FloaT -> ""

pplist :: PP x => [x] -> Doc
pplist = sep . punctuate comma . map pp

pphlist :: HetList f -> [Doc]
pphlist HNil = []
pphlist (HCons b bs) = pp b : pphlist bs

braceBlock :: Doc -> Doc
braceBlock d = lbrace $+$ nest 4 d $+$ rbrace

semiBind :: HetList Binding -> Doc
semiBind = vcat . punctuate semi . pphlist

------------------------------------------------------------------------------
-- Error messages ------------------------------------------------------------
errNonHasRep = "Non-scalar type as vector or matrix element."

