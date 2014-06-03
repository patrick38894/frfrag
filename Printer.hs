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
semisep = sep . punctuate semi
braceblock x = lbrace $+$ nest 4 x $+$ rbrace

pptagged :: Tagged -> Doc
pptagged (Tagged a) = pp a

ppbinding :: Bind -> Doc
ppbinding (Var t i) = pptag t <+> ppname (Var t i)

ppname :: Bind -> Doc
ppname (Var t i) = text "var" <> PP.int i

ppassign :: Bind -> TagExpr -> Doc
ppassign b e = ppbinding b <+> equals <+> ppexpr e <> semi

ppargs :: [Bind] -> Doc
ppargs = commasep . map ppbinding

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

ppexpr :: TagExpr -> Doc
ppexpr e = case e of
    Lit t l -> pptagged l 
    GMat t (Mat m) -> pptag t <> parens (commasep $ map ppexpr $ concat m)
    MulOp s _ x y -> ppexpr x <+> text s <+> ppexpr y
    AddOp s _ x y -> parens (ppexpr x <+> text s <+> ppexpr y)
    CompOp s x y -> ppexpr x <+> text s <+> ppexpr y
    Prim s _ _ x -> text s <> parens (ppexpr x) 
    Prim2 s _ _ _ x y -> text s <> parens (ppexpr x <> comma <+> ppexpr y)
    Call b x -> undefined
    Val b -> ppname b
    Swiz b _ s -> ppname b <> text "." <> text s

ppstmt :: TagStmt -> Doc
ppstmt s = case s of
    Param _ -> empty
    DecVal i t e -> ppassign (Var t i) e <> semi
    Mutate b e -> ppname b <+> equals <+> ppexpr e <> semi
    Block s -> braceblock $ commasep $ map ppstmt s
    IfElse p i e -> text "if" <> parens (ppexpr p)
                    <+> braceblock (ppstmt i) 
                    <+> text "else" <+> braceblock (ppstmt e) 
    For b i p t s  -> text "for"
                    <> (parens $ semisep $ [ppassign b i, ppexpr p, ppassign b t])
                    <+> braceblock (ppstmt s)

    While e s -> text "while" <> parens (ppexpr e) <+> braceblock (ppstmt s)
    Ret e -> text "return" <+> ppexpr e <> semi
    Discard -> text "discard;"
    Halt -> text "return;"
    Break -> text "break;"
    Cont -> text "continue;"
    NoOp -> empty

ppdecl :: TagDecl -> Doc
ppdecl d = case d of
    Uni i t e ->  let v = (Var t i) 
        in text "uniform" <+> case e of
        Nothing -> ppbinding v <> semi
        Just x -> ppassign (Var t i) x
    Value i t e -> ppassign (Var t i) e
    Proc i t ts s -> ppname (Var t i) <> parens (ppargs ts) <+> braceblock (ppstmt s)
    Main s -> text "main()" <+> braceblock (ppstmt s)

