{-# Language FlexibleInstances, GADTs #-}
import Text.PrettyPrint.HughesPJ
import Language

commasep :: [Doc] -> Doc
commasep = sep . punctuate comma 

ppN :: N -> Doc
ppN = text . show . intN

ppVecN :: Show a => VecN a -> Doc
ppVecN = commasep . map (text . show) . vecToList

ppRep :: Rep a r -> Doc
ppRep r = case r of
    VoidT       -> text "void"
    BoolT       -> text "bool"
    IntT        -> text "int"
    FloatT      -> text "float"
    VecT r n    -> text (getInitial r) <> ppN n
    FuncT r a   -> error "First class function type not implemented"

getInitial :: Rep () a -> String
getInitial r = case r of
    BoolT -> "b"
    IntT -> "i"
    FloatT -> ""

ppBinding :: Binding a r -> Doc
ppBinding b = case b of
    Void        -> empty
    FragCoord   -> text "gl_FragCoord"
    FragColor   -> text "gl_FragColor"
    Var r s     -> ppRep r <+> text s 
    Func a r    -> ppProto r [ppCheckArg ppBinding a]

ppName :: Binding a r -> Doc
ppName b = case b of
    Var r s     -> text s
    Func a r    -> ppName r 
    other       -> ppBinding other

ppProto :: Binding a r -> [Doc] -> Doc
ppProto r as = case r of
    v @ (Var r c) -> ppBinding v <> parens (commasep as)
    Func r' a'    -> ppProto r' (as ++ [ppCheckArg ppBinding a'])
    other         -> error "Invalid argument to function"

ppCheckArg :: (Binding a r -> Doc) -> Binding a r -> Doc
ppCheckArg f a = case a of
    Var r s         -> text "const" <+> f a
    Func r x        -> error "Cannot give function as function argument"
    FragCoord       -> error "Cannot give 'gl_FragCoord' as function argument"
    FragColor       -> error "Cannot give 'gl_FragColor' as function argument"

ppExpr :: Expr a r -> Doc
ppExpr e = case e of
    Float x         -> float x
    Int x           -> int x
    Bool x          -> text $ if x then "true" else "false"
    Vec r x         -> ppRep r <> parens (commasep $ map ppExpr (vecToList x))
    Val v           -> ppName v 
    App f a         -> ppApply f [ppExpr a]
    other           -> error "Invalid expression"

ppApply :: Expr a r -> [Doc] -> Doc
ppApply f as = case f of
    App g a'        -> ppApply g (as ++ [ppExpr a'])
    Call g          -> ppName g <> parens (commasep as)
    Prim s          -> text s <> parens (commasep as)
    Prim2 s         -> text s <> parens (commasep as)
    BinOp s         -> case as of [y,x] -> parens (x <+> text s <+> y)
