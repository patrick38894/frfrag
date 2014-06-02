module Printer where
import Language
import Text.PrettyPrint.HughesPJ

class PPNative a where pp :: a -> Doc
instance PPNative Int where pp = int
instance PPNative Bool where pp x = text $ if x then "true" else "false"
instance PPNative Float where pp = float
instance PPNative a => PPNative [a] where
    pp = sep . punctuate comma . map pp


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


