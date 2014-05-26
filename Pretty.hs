{-# Language
             FlexibleContexts,
             FlexibleInstances,
             GADTs,
             RankNTypes,
             TypeSynonymInstances,
             UndecidableInstances
 #-}

module Pretty where
import Expressions
import Synonyms
import Vector
import HigherOrder
import Text.PrettyPrint.HughesPJ hiding (float, int)

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
