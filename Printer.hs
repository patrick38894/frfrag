{-# Language FlexibleInstances, OverlappingInstances, UndecidableInstances #-}
module Printer where
import Language
import Text.PrettyPrint.HughesPJ hiding (int, float)
import qualified Text.PrettyPrint.HughesPJ as PP

-- Pretty printing
instance Show (WriteProg()) where show = render . vcat . map ppdecl . runProg
instance Show TagDecl where show = render . ppdecl

-- Shorthand for printing showable things
class PPNative a where pp :: a -> Doc
instance PPNative Int where pp = PP.int
instance PPNative Bool where pp x = text $ if x then "true" else "false"
instance PPNative Float where pp = PP.float
instance Show a => PPNative a where pp = text . show
instance PPNative a => PPNative (Mat a) where
    pp (Mat xs) = commasep $ map pp $ concat $ xs

-- Shorthand for laying out lists, etc
commasep = sep . punctuate comma
semisep = sep . punctuate semi
braceblock x = lbrace $+$ nest 4 x $+$ rbrace

-- Print a type-erased value
pptagged :: Tagged -> Doc
pptagged (Tagged a) = pp a

-- Print out a binding, including type and name
ppbinding :: Bind -> Doc
ppbinding (Var t i) = pptag t <+> ppname (Var t i)
ppbinding FragColor = ppname FragColor
ppbinding FragCoord = ppname FragCoord

-- Print out just the name of a binding
ppname :: Bind -> Doc
ppname n = case n of
    Var t i -> text "var" <> PP.int i
    FragColor -> text "gl_FragColor"
    FragCoord -> text "gl_FragCoord"

-- Print out assignments and declarations
-- (declarations show type)
ppassign, ppdeclare :: Bind -> TagExpr -> Doc
ppassign b e = ppname b <+> equals <+> ppexpr e
ppdeclare b e = ppbinding b <+> equals <+> ppexpr e

-- Print out an argument list
ppargs :: [Bind] -> Doc
ppargs = commasep . map ppbinding

-- Print out an else clause
-- (blank if the else is a no op)
ppelse :: TagStmt -> Doc
ppelse e = case e of 
    Block [NoOp] -> empty
    Block [] -> empty
    NoOp -> empty
    x -> text " else" $+$ ppstmt e

-- Print out a type
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

-- Print an expr,
-- laying it out like in GLSL
ppexpr :: TagExpr -> Doc
ppexpr e = case e of
    Lit t l -> pptagged l 
    GMat t (Mat m) -> pptag t <> parens (commasep $ map ppexpr $ concat m)
    MulOp s _ x y -> ppexpr x <+> text s <+> ppexpr y
    AddOp s _ x y -> parens (ppexpr x <+> text s <+> ppexpr y)
    CompOp s x y -> ppexpr x <+> text s <+> ppexpr y
    Prim s _ _ x -> text s <> parens (ppexpr x) 
    Prim2 s _ _ _ x y -> text s <> parens (ppexpr x <> comma <+> ppexpr y)
    Call b x -> ppname b <> parens (ppexpr x)
    Val b -> ppname b
    Swiz b _ s -> ppexpr b <> text "." <> text s
    IfExpr _ p i e -> sep [ppexpr p <> text "?", parens (ppexpr i), 
                          text ":", parens (ppexpr e)]

-- Statements
ppstmt :: TagStmt -> Doc
ppstmt s = case s of
    Param _ -> empty
    DecVal i t e -> ppdeclare (Var t i) e <> semi
    Mutate b e -> ppname b <+> equals <+> ppexpr e <> semi
    Block s -> vcat $ map ppstmt s
    IfElse p i e -> sep [text "if" <> parens (ppexpr p), ppstmt i]
                    <> ppelse e
    For b i p t s  -> text "for"
                    <> (parens $ semisep $ [ppdeclare b i, ppexpr p, ppassign b t])
                    $+$ braceblock (ppstmt s)

    While e s -> text "while" <> parens (ppexpr e) $+$ braceblock (ppstmt s)
    Ret e -> text "return" <+> ppexpr e <> semi
    Discard -> text "discard;"
    Halt -> text "return;"
    Break -> text "break;"
    Cont -> text "continue;"
    NoOp -> empty

-- Declarations
ppdecl :: TagDecl -> Doc
ppdecl d = case d of
    Uni i t e ->  let v = (Var t i) 
        in text "uniform" <+> case e of
        Nothing -> ppbinding v <> semi
        Just x -> ppdeclare (Var t i) x <> semi
    Value i t e -> ppdeclare (Var t i) e <> semi
    Proc i t ts s -> ppbinding (Var t i) <> parens (ppargs ts) $+$ braceblock (ppstmt s)
    Main s -> text "void main()" $+$ braceblock (ppstmt s)

