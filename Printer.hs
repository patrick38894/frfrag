{-# Language FlexibleInstances, OverlappingInstances, UndecidableInstances #-}
module Printer where
import Language
import Text.PrettyPrint.HughesPJ hiding (int, float)
import qualified Text.PrettyPrint.HughesPJ as PP

class PPNative a where pp :: a -> Doc
instance PPNative Int where pp = PP.int
instance PPNative Bool where pp x = text $ if x then "true" else "false"
instance PPNative Float where pp = PP.float
instance Show a => PPNative a where pp = text . show
instance PPNative a => PPNative (Mat a) where
    pp (Mat xs) = commasep $ map pp $ concat $ xs

commasep = sep . punctuate comma

pptag :: Type -> Doc
pptag t = case t of
    Type Int 1 1 -> text "int"
    Type Float 1 1 -> text "float"
    Type Bool 1 1 -> text "bool"    
    Type a n m -> let 
        p = case a of
                Bool -> "b" 
                Int -> "i"
                Float -> ""
        s = if n == 1 || m == 1
                then ("vec" ++ show (max n m))
                else if n == m 
                    then ("mat" ++ show n)
                    else ("mat" ++ show n ++ "x" ++ show m)
        in text $ p ++ s

pptagged :: Tagged -> Doc
pptagged (Tagged a) = pp a

ppexpr :: TagExpr -> Doc
ppexpr e = case e of
    Lit t l -> pptagged l 
    GMat t (Mat m) -> pptag t <> parens (commasep $ map ppexpr $ concat m)
