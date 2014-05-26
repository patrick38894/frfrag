{-# Language
             FlexibleInstances,
             GADTs,
             TypeSynonymInstances,
             UndecidableInstances
 #-}
module Pretty where
import CoreLanguage
import Synonyms
import Vector
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
    FuncT r a   -> error "No native GLSL function type"
    VecT r n    -> getInitial r <> pp "vec" <> pp n
    MatT r n m  -> getInitial r <> pp "mat" <> pp n
                    <> if m == n then empty else pp "x" <> pp m
    where getInitial r = pp $ case r of 
            BoolT   -> "b"
            IntT    -> "i"
            FloaT   -> ""

instance Pretty (Binding t) where
  pp bind = case bind of
    FragCoord       -> pp "gl_FragCoord"
    FragColor       -> pp "gl_FragColor"
    Var r nm        -> pp r <+> pp nm
    Swiz b s        -> pp b <> period <> pp s
    Func r a        -> case r of
        Var ret cal -> pp ret <+> pp cal <+> lparen <> pp a
        Func r' a'  -> pp (Func r' a) <> comma <+> pp a'
        other       -> error "Invalid argument to function."


instance Pretty (Decl t) where
  pp decl = case decl of
    Value b e       -> pp b <+> equals <+> pp e
    Uniform b e     -> pp b <> (case e of
                        Just x -> empty <+> equals <+> pp x
                        Nothing -> empty ) <> semi  
    Procedure nm r stmt -> undefined
    Function nm r expr -> undefined  

instance Pretty (Expr t)

instance Show N where show = render . pp
instance Pretty a => Show (VecN a) where show = render . pp
instance Pretty a => Show (MatN a) where show = render . pp
instance Pretty (Rep a) => Show (Rep a) where show = render . pp
instance Pretty (Binding a) => Show (Binding a) where show = render . pp

period = pp "."
commasep :: [Doc] -> Doc
commasep = sep . punctuate comma
commaseq :: Pretty a => [a] -> Doc
commaseq = commasep . map pp
