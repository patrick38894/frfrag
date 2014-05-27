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
    Float :: Float -> Expr Float
    Bool :: Bool -> Expr Bool
    Int :: Int -> Expr Int
    Vec :: Rep (VecN t) -> VecN t -> Expr (VecN t)
    Mat :: Rep (MatN t) -> MatN t -> Expr (MatN t)
    Prim :: String -> Expr (a -> b)
    Prim2 :: String -> Expr (a -> b -> c)
    BinOp :: String -> Expr (a -> b -> c)
    Val :: Binding t -> Expr t
    Call :: Binding (t -> u) -> Expr (t -> u)
    Rewrite :: Expr t -> Expr u -> Expr (t -> u)
    Sym :: Rep a -> Int -> Expr a
    App :: (Pretty t, Wrap Expr t, Wrap Rep t) => Expr (t -> u) -> Expr t -> Expr u
    Last :: Stream (Expr a) -> Expr a

data Stream a where
    Sequence :: Int -> Int -> Int -> Stream (Expr Int)
    Map :: Expr (a -> b) -> Stream (Expr a) -> Stream (Expr b)
    Take :: Expr (x -> Bool) -> Stream (Expr x) -> Stream (Expr x)
    Scan :: Expr (x -> a -> a) -> Expr a -> Stream (Expr x) -> Stream (Expr a)
    Cat :: Stream (Expr a) -> Stream (Expr a) -> Stream (Expr a)

------------------------------------------------------------------------------

class Wrap f a where wrap :: a -> f a
class Extract f where extract :: f a -> a

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
data SyntaxError = SyntaxError String
class Pretty a where pp :: a -> Doc

instance Pretty String where pp = text
instance Pretty SyntaxError where pp (SyntaxError a) = pp "GLSL AST ERROR:" <+> pp a
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
    PolyT       -> pperror "Polymorphic type not implemented"
    FuncT r a   -> pperror "First-class function type not implemented"
    where getInitial r = pp $ case r of 
            BoolT   -> "b"
            IntT    -> "i"
            FloaT   -> ""

instance Pretty (Binding t) where
  pp bind = case bind of
    FragCoord       -> pp "gl_FragCoord"
    FragColor       -> pp "gl_FragColor"
    Var r nm        -> pp r <+> pp nm
    Func r a        -> ppfunc r [ checkArgType pp a ]
    Swiz b s        -> (case b of Var r nm -> pp nm
                                  Func r a -> pperror "Function swizzling not implemented"
                                  other -> pp other) <> period <> pp s
    Void            -> empty

instance Pretty (a->b) where pp = const $ pperror "Cannot print native or GLSL function"

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
    App f a         -> ppapply f [pp a]
    Prim s          -> pperror $ "Cannot call unary '" ++ s ++ "' without arguments"
    Prim2 s         -> pperror $ "Cannot call binary '" ++ s ++ "' without arguments"
    BinOp s         -> pperror $ "Cannot call binary op '" ++ s ++ "' without arguments"
    Call b          -> pperror $ "Cannot call function '" ++ show b ++ "' without arguments"
    Rewrite r e     -> pperror $ "Cannot print partially applied abstraction"
    Sym i r         -> pperror $ "Cannot print symbol '(" ++ show i ++ ")'"

instance Show N where show = render . pp
instance Pretty a => Show (VecN a) where show = render . pp
instance Pretty a => Show (MatN a) where show = render . pp
instance Pretty (Rep a) => Show (Rep a) where show = render . pp
instance Pretty (Binding a) => Show (Binding a) where show = render . pp
instance Pretty (Expr a) => Show (Expr a) where show = render . pp

pperror = pp . SyntaxError
period = pp "."
commasep :: [Doc] -> Doc
commasep = sep . punctuate comma
commaseq :: Pretty a => [a] -> Doc
commaseq = commasep . map pp
braceblock :: Doc -> Doc
braceblock x = lbrace $+$ nest 4 x $+$ rbrace

checkArgType f a = case a of
    Var r s -> pp "const" <+> f a
    Func r x -> pperror $ "Cannot give function " ++ show (Func r x) ++ " as function argument"
    FragCoord -> pperror "Cannot give 'gl_FragCoord' as function argument"
    FragColor -> pperror "Cannot give 'gl_FragColor' as function argument"
    Swiz v s -> pperror $ "Cannot give swizzle '" ++ show (Swiz v s) ++ "' as function argument"

ppfunc :: Binding t -> [Doc] -> Doc
ppfunc r a = case r of
    v @ (Var ret cal) -> pp v <> parens (commasep a)
    Func r' a'      -> ppfunc r' (a ++ [checkArgType pp a'])
    other           -> pperror "Invalid argument to function"

ppapply :: (Wrap Rep a, Wrap Expr a, Pretty a) => Expr (a -> b) -> [Doc] -> Doc
ppapply f a = case f of
        Call g -> ppname g <> parens (commasep a)
        Rewrite r e -> pperror "(Function application NOT YET IMPLEMENTED)"
        App g e -> ppapply g (a ++ [pp e])
        Prim s -> pp s <> parens (commasep a)
        Prim2 s -> pp s <> parens (commasep a)
        BinOp s -> case a of [x,y] -> parens (x <+> pp s <+> y)

ppname :: Binding t -> Doc
ppname b = case b of
    Var ret cal -> pp cal
    Func ret arg -> ppname ret
