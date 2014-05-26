{-# Language 
             FlexibleContexts,
             FlexibleInstances,
             GADTs,
             KindSignatures,
             MultiParamTypeClasses,
             RankNTypes,
             TypeSynonymInstances,
             UndecidableInstances
 #-}

module Expressions where
import Vector
import Text.PrettyPrint.HughesPJ hiding (float, int)

------------------------------------------------------------------------------
-- GLSL language representation ----------------------------------------------
data Rep :: * -> * where
    BoolT :: Rep Bool
    IntT :: Rep Int
    FloaT :: Rep Float
    FuncT :: Rep r -> Rep a -> Rep (a -> r)
    VecT :: Rep t -> N -> Rep (VecN t)
    MatT :: Rep t -> N -> N -> Rep (MatN t)
    PolyT :: Rep a
    VoidT :: Rep ()

data Binding :: * -> * where
    FragCoord :: Binding (VecN Float)
    FragColor :: Binding (VecN Float)
    Var :: Rep t -> String -> Binding t
    Swiz :: Binding v -> String -> Binding x
    Func :: Binding r -> Binding a -> Binding (a -> r)
    Void :: Binding ()

data Expr :: * -> * where
--  Core GLSL expressions
--  Literals
    Float :: Float -> Expr Float
    Bool :: Bool -> Expr Bool
    Int :: Int -> Expr Int
    Vec :: Rep (VecN t) -> VecN t -> Expr (VecN t)
    Mat :: Rep (MatN t) -> MatN t -> Expr (MatN t)
--  Primitive unary and binary functions and ops
    Prim :: String -> Expr (a -> b)
    Prim2 :: String -> Expr (a -> b -> c)
    BinOp :: String -> Expr (a -> b -> c)
--  GLSL Abstractions
    Val :: Binding t -> Expr t
    Call :: Binding (t -> u) -> Expr (t -> u)
--  Fake expressions (cannot be printed directly). 
--  Term-rewriting abstractions
    Lam :: Int -> Rep t -> Expr u -> Expr (t -> u)
    LamT :: Int -> Expr a
    App :: (Pretty t, Wrap Expr t, Wrap Rep t) => Expr (t -> u) -> Expr t -> Expr u
    Last :: Stream (Expr a) -> Expr a

data Stream a where
    Sequence :: Int -> Int -> Int -> Stream (Expr Int)
    Map :: Expr (a -> b) -> Stream (Expr a) -> Stream (Expr b)
    Take :: Expr (x -> Bool) -> Stream (Expr x) -> Stream (Expr x)
    Scan :: Expr (x -> a -> a) -> Expr a -> Stream (Expr x) -> Stream (Expr a)

------------------------------------------------------------------------------

class Wrap f a where wrap :: a -> f a
class Extract f where extract :: f a -> a

class HFunctor f where 
    hfmap :: Wrap f b => (a -> b) -> f a -> f b

instance (Extract f) => HFunctor f where
    hfmap f = wrap . f . extract
 
instance Wrap Rep Int where wrap = const IntT
instance Wrap Rep Float where wrap = const FloaT
instance Wrap Rep Bool where wrap = const BoolT
instance Wrap Rep a => Wrap Rep (VecN a) where
    wrap v = VecT (wrap $ vX v) (vecSize v)

instance Extract Expr where 
  extract e = case e of
    Float x -> x
    Int x -> x
    Vec r x -> x
    Mat r x -> x

instance Wrap Expr Int where wrap = Int
instance Wrap Expr Float where wrap = Float
instance Wrap Expr Bool where wrap = Bool
instance (Wrap Rep a, Wrap Expr a) => Wrap Expr (VecN a) where
    wrap v = Vec (case v of Vec2 a _ -> VecT (wrap a) N2
                            Vec3 a _ _ -> VecT (wrap a) N3
                            Vec4 a _ _ _ -> VecT (wrap a) N4) v
instance Wrap Rep (a -> b) where wrap = error "No GLSL representation for function types"
instance Wrap Expr (a -> b) where wrap = error "No GLSL representation for function types"

------------------------------------------------------------------------------

class Pretty a where pp :: a -> Doc

instance Pretty String where pp = text
instance Pretty Int where pp = pp . show
instance Pretty Float where pp = pp . show
instance Pretty Bool where pp b = pp $ if b then "true" else "false"
instance Pretty N where pp = pp . asInt
instance Pretty a => Pretty (VecN a) where pp = commaseq. vecToList
instance Pretty a => Pretty (MatN a) where
  pp (MatN m) = commaseq . concatMap vecToList . vecToList $ m

instance Pretty (Rep t) where
  pp r = case r of
    BoolT       -> pp "bool"
    FloaT       -> pp "float"
    IntT        -> pp "int"
    VecT r n    -> getInitial r <> pp "vec" <> pp n
    MatT r n m  -> getInitial r <> pp "mat" <> pp n
                    <> if m == n then empty else pp "x" <> pp m
    VoidT       -> pp "void"
    PolyT       -> error "No native GLSL polymorphic type"
    FuncT r a   -> error "No native GLSL function type"
    where getInitial r = pp $ case r of 
            BoolT   -> "b"
            IntT    -> "i"
            FloaT   -> ""

instance Pretty (Binding t) where
  pp bind = case bind of
    FragCoord       -> pp "gl_FragCoord"
    FragColor       -> pp "gl_FragColor"
    Var r nm        -> pp r <+> pp nm
    Func r a        -> ppfunc r [pp a]
    Swiz b s        -> (case b of Var r nm -> pp nm
                                  Func r a -> error "Cannot swizzle function"
                                  other -> pp other) <> period <> pp s
    Void            -> empty

ppfunc :: Binding t -> [Doc] -> Doc
ppfunc = ppcurry pp

ppcurry :: (forall a . Binding a -> Doc) -> Binding t -> [Doc] -> Doc
ppcurry f r a = case r of
    v @ (Var ret cal) -> f v <> parens (commasep a)
    Func r' a'      -> ppcurry f r' (f a' : a)
    other           -> error "Invalid argument to function."

instance Pretty (a->b) where pp = error "Cannot print function"
instance (Wrap Expr t, Wrap Rep t, Pretty t) => Pretty (Expr t) where
  pp e = case e of
    Float f         -> pp f
    Bool b          -> pp b
    Int i           -> pp i
    Vec r v         -> pp r <> parens (pp v)
    Mat r v         -> pp r <> parens (pp v)
    Val b           -> case b of
                        Var r nm  -> pp nm
                        Func r a -> error "Cannot access function as value"
                        other -> pp other
    App f a         -> ppapply f a
    other           -> error "Cannot print partially applied function"

ppapply :: (Wrap Rep a, Wrap Expr a, Pretty a) => Expr (a -> b) -> Expr a -> Doc
ppapply f a = case f of
        Call g -> ppfunc g [pp a]
        Lam i r e -> undefined
        App g e -> undefined 
        other -> error "Invalid function application target"

instance Show N where show = render . pp
instance Pretty a => Show (VecN a) where show = render . pp
instance Pretty a => Show (MatN a) where show = render . pp
instance Pretty (Rep a) => Show (Rep a) where show = render . pp
instance Pretty (Binding a) => Show (Binding a) where show = render . pp
instance Pretty (Expr a) => Show (Expr a) where show = render . pp

period = pp "."
commasep :: [Doc] -> Doc
commasep = sep . punctuate comma
commaseq :: Pretty a => [a] -> Doc
commaseq = commasep . map pp
braceblock :: Doc -> Doc
braceblock = braces . nest 4
