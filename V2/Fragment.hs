{-# Language ExistentialQuantification, 
             FlexibleInstances,
             GADTs,
             KindSignatures,
             RankNTypes,
             StandaloneDeriving,
             TypeSynonymInstances #-}

module V2.Fragment where
import Text.PrettyPrint.HughesPJ
import Data.Char
import GHC.Exts


------------------------------------------------------------------------------
-- Structured GLSL program representation ------------------------------------

data Fragment = Frag 
                {
                    uniform :: HetList Binding,
                    decls :: HetList Decl,
                    main :: Stmt
                }


------------------------------------------------------------------------------
-- GLSL language representation ----------------------------------------------
data SmallNat = N2 | N3 | N4 deriving (Eq, Enum, Ord)

data Rep :: * -> * where
    BoolT :: Rep Bool
    IntT :: Rep Int
    FloaT :: Rep Float
    VecT :: SmallNat -> Rep t -> Rep [t]
    MatT :: SmallNat -> SmallNat -> Rep t -> Rep [[t]]
    RecT :: String -> ArgList -> Rep String

data Binding :: * -> * where
    Val :: String -> Rep t -> Binding t
    Proc :: String -> Rep t -> ArgList -> Binding (ArgList -> t)
    Rec :: String -> ArgList -> Binding (ArgList -> String) 

type ArgList = HetList Binding
type Args = HetList Expr

data HetList f where
    Nil :: HetList f
    Cons :: PP (f a) => f a -> HetList f -> HetList f

data Expr :: * -> * where
    Float :: Float -> Expr Float
    Bool :: Bool -> Expr Bool
    Int :: Int -> Expr Int
    Vec :: (PP t, HasRep t) => [t] -> Expr [t]
    Mat :: (PP t, HasRep t) => [[t]] -> Expr [[t]]
    Var :: Binding t -> Expr t
    Call :: Binding ret -> Args -> Expr ret
    Con :: Binding rec -> Args -> Expr rec
    Dot :: Binding rec -> Binding a -> Expr a
    Swiz :: Expr [t] -> Expr [t]

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

vec2 = VecT N2 FloaT
vec3 = VecT N3 FloaT
vec4 = VecT N4 FloaT
ivec2 = VecT N2 IntT
ivec3 = VecT N3 IntT
ivec4 = VecT N4 IntT
bvec2 = VecT N2 BoolT
bvec3 = VecT N3 BoolT
bvec4 = VecT N4 BoolT

class HasRep a where
    getRep :: Rep a

instance HasRep Int where getRep = IntT
instance HasRep Float where getRep = FloaT
instance HasRep Bool where getRep = BoolT

getVecRep :: HasRep a => [a] -> Rep [a]
getVecRep xs = VecT (getDimEnum $ length xs) getRep
getMatRep :: HasRep a => [[a]] -> Rep [[a]]
getMatRep xs = MatT (getDimEnum $ length xs) 
                   (getDimEnum . the $ map length xs) getRep
getDimEnum :: Int -> SmallNat
getDimEnum n = toEnum (n-1)


------------------------------------------------------------------------------
-- Pretty printing -----------------------------------------------------------

pplist :: PP x => [x] -> Doc
pplist = sep . punctuate comma . map pp

pphlist :: HetList f -> [Doc]
pphlist Nil = []
pphlist (Cons b bs) = pp b : pphlist bs

braceBlock :: Doc -> Doc
braceBlock d = lbrace $+$ nest 4 d $+$ rbrace

instance PP (HetList f) where
    pp = sep . punctuate comma . pphlist

semiBind :: HetList Binding -> Doc
semiBind = vcat . punctuate semi . pphlist


instance Show SmallNat where show = render . pp
instance Show (Rep a) where show = render . pp

class PP x where
    pp :: x -> Doc

instance PP String where
    pp = text

instance PP SmallNat where
    pp n = pp . show $ case n of N2 -> 2; N3 -> 3; N4 -> 4

instance PP (Rep a) where
    pp r = case r of
        BoolT        -> pp "bool"
        FloaT       -> pp "float"
        IntT         -> pp "int"
        VecT n r     -> repInit r <> pp "vec" <> pp n
        MatT n m r   -> repInit r <> pp "mat" <> pp n 
                        <> if m == n then empty else pp "x" <> pp m
        RecT nm fs  -> pp "struct" <+> pp nm $+$ braceBlock (semiBind fs)
        where repInit r = pp $ case r of 
                                    BoolT -> "b"
                                    IntT -> "i"
                                    FloaT -> ""

instance PP a => PP (Expr a) where
    pp expr = case expr of
        Float n    -> pp (show n)
        Bool b     -> pp (if b then "true" else "false")
        Int n      -> pp (show n)
        Vec ts     -> pp (getVecRep ts) <> parens (pplist ts)
        Mat ts     -> pp (getMatRep ts) <> (parens . pplist . concat) ts
        Var (Val s r) -> pp s
        Call (Proc s r a) p -> pp s <> (parens (pp p))

instance PP (Binding t) where
    pp (Val s r) = pp r <+> pp s

instance PP t => PP (Decl t) where
    pp dec = case dec of
        Value (Val s r) e -> pp r <+> pp s <+> equals <+> pp e
        Process (Proc s r a) b -> pp r <+> pp s <+> parens (pp a) 
                                 $+$ braceBlock (pp b)
        -- Data s a e -> pp r <+> pp s <+> parens (pp a) $+$ braceBlock (pp r)

instance PP Stmt where
    pp stmt = case stmt of
        NoOp -> empty
        Block stmts -> braceBlock (vcat (map pp stmts))

------------------------------------------------------------------------------
-- Error messages ------------------------------------------------------------
errNonHasRep = "Non-scalar type as vector or matrix element."

