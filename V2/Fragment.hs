{-# Language ExistentialQuantification, 
             FlexibleInstances,
             GADTs,
             KindSignatures,
             StandaloneDeriving,
             TypeSynonymInstances #-}

module V2.Fragment where
import Text.PrettyPrint.HughesPJ
import Data.Char
import GHC.Exts


{-data Frag = Frag {uniforms        :: [Uniform], 
                  declarations    :: [Decl], 
                  fragMain        :: GLStmt} 

-}

------------------------------------------------------------------------------
-- Representations of types --------------------------------------------------

data SmallNat = N2 | N3 | N4 deriving (Eq, Enum, Ord)

data Rep :: * -> * where
    Bool :: Rep Bool
    Int :: Rep Int
    Float :: Rep Float
    Vec :: SmallNat -> Rep a -> Rep [a]
    Mat :: SmallNat -> SmallNat -> Rep a -> Rep [[a]]

class GetRep a where
    getRep :: a -> Rep a

instance GetRep Int where getRep = const Int
instance GetRep Float where getRep = const Float
instance GetRep Bool where getRep = const Bool

getVecRep :: GetRep a => [a] -> Rep [a]
getMatRep :: GetRep a => [[a]] -> Rep [[a]]
getVecRep xs = Vec (getDimEnum $ length xs) (getRep (head xs))
getMatRep xs = Mat (getDimEnum $ length xs) 
                   (getDimEnum . the $ map length xs) (getRep (head (head xs)))
getDimEnum :: Int -> SmallNat
getDimEnum n = toEnum (n-1)

-- Vector type synonyms are provided for convenience.
vec2 = Vec N2 Float
vec3 = Vec N3 Float
vec4 = Vec N4 Float
ivec2 = Vec N2 Int
ivec3 = Vec N3 Int
ivec4 = Vec N4 Int
bvec2 = Vec N2 Bool
bvec3 = Vec N3 Bool
bvec4 = Vec N4 Bool

------------------------------------------------------------------------------
-- Expressions ---------------------------------------------------------------

data Exp :: * -> * where
    BoolL :: Bool -> Exp Bool
    FloatL :: Float -> Exp Float
    IntL :: Int -> Exp Int
    VecE :: (PP t, GetRep t) => [t] -> Exp [t]
    MatE :: (PP t, GetRep t) => [[t]] -> Exp [[t]]
    VarE :: Binding t -> Exp t
    CallE :: Binding r -> Args -> Exp (FArgs -> r)

data Binding t = Bind String (Rep t)

-- Heterogenous, variable length argument lists.    
data Args = forall a. Arg (Binding a) Args | NoMoreArgs
data FArgs = forall a . FArg (Binding a) (Exp a) FArgs | NoMoreFArgs

------------------------------------------------------------------------------
-- Declarations and statements -----------------------------------------------

data Dec :: * -> * where
    Val :: Binding t -> Exp t -> Dec t
    Func :: Binding t -> Args -> Exp t -> Dec (FArgs -> t)
    Proc :: Binding t -> Args -> Stmt -> Dec (FArgs -> t)

data Stmt where
    NoOp :: Stmt
    Block :: [Stmt] -> Stmt
    Var :: Binding t -> Exp t -> Stmt
    Asgn :: Binding t -> Exp t -> Stmt
    Extract :: Binding r -> Binding r -> FArgs -> Stmt 
    IfElse :: Exp Bool -> Stmt -> Stmt -> Stmt
    For :: Binding i -> Exp i -> Exp (i -> Bool) -> Exp (i -> i) -> Stmt -> Stmt
    While :: Exp Bool -> Stmt -> Stmt
    Break :: Stmt
    Continue :: Stmt
    Return :: Exp a -> Stmt
    Terminate :: Stmt
    Discard :: Stmt

------------------------------------------------------------------------------
-- Pretty printing -----------------------------------------------------------

arglist :: PP x => [x] -> Doc
arglist = sep . punctuate comma . map pp

braceBlock :: Doc -> Doc
braceBlock d = lbrace $+$ nest 4 d $+$ rbrace

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
        Bool        -> pp "bool"
        Float       -> pp "float"
        Int         -> pp "int"
        Vec n r     -> repInit r <> pp "vec" <> pp n
        Mat n m r   -> repInit r <> pp "mat" <> pp n 
                    <> if m == n then empty else pp "x" <> pp m
        where repInit r = pp $ case r of 
                                    Bool -> "b"
                                    Int -> "i"
                                    Float -> ""
                                    x -> error errNonScalar

instance PP (Exp a) where
    pp expr = case expr of
        BoolL b     -> pp (if b then "true" else "false")
        FloatL n    -> pp (show n)
        IntL n      -> pp (show n)
        VecE ts     -> pp (getVecRep ts) <> parens (arglist ts)
        MatE ts     -> pp (getMatRep ts) <> (parens . arglist . concat) ts
        VarE (Bind s r) -> pp s
        CallE (Bind s r) a -> pp s <> (parens . pp) a

instance PP (Binding t) where
    pp (Bind s r) = pp r <+> pp s

instance PP Args where
    pp args = case args of
        Arg x NoMoreArgs -> pp x
        Arg x xs -> pp x <> comma <+> pp xs

instance PP FArgs where
    pp fargs = case fargs of
        FArg x e NoMoreFArgs -> pp e 
        FArg x e xs -> pp e <> comma <+> pp xs

instance PP (Dec t) where
    pp dec = case dec of
        Val (Bind s r) e -> pp r <+> pp s <+> equals <+> pp e
        Func (Bind s r) a e -> pp r <+> pp s <+> parens (pp a) $+$ braceBlock (pp r)
        Proc (Bind s r) a b -> pp r <+> pp s <+> parens (pp a) $+$ braceBlock (pp r)

instance PP Stmt where
    pp stmt = case stmt of
        NoOp -> empty

------------------------------------------------------------------------------
-- Error messages ------------------------------------------------------------
errNonScalar = "Non-scalar type as vector or matrix element."

