{-# Language FlexibleInstances, GADTs #-}
import Text.PrettyPrint.HughesPJ
import Language
import Vector

commasep :: [Doc] -> Doc
commasep = sep . punctuate comma 
braceblock :: Doc -> Doc
braceblock d  = lbrace $+$ nest 4 d $+$ rbrace

ppN :: N -> Doc
ppN = text . show . intN

ppVecN :: Show a => VecN a -> Doc
ppVecN = commasep . map (text . show) . vecToList

ppRep :: Rep r a -> Doc
ppRep r = case r of
    VoidT       -> text "void"
    BoolT       -> text "bool"
    IntT        -> text "int"
    FloatT      -> text "float"
    VecT r n    -> text (getInitial r) <> ppN n
    FuncT r a   -> error "First class function type not implemented"

getInitial :: Rep r () -> String
getInitial r = case r of
    BoolT -> "b"
    IntT -> "i"
    FloatT -> ""

ppBinding :: Binding r a -> Doc
ppBinding b = case b of
    Void        -> empty
    FragCoord   -> text "gl_FragCoord"
    FragColor   -> text "gl_FragColor"
    Var r s     -> ppRep r <+> text s 
    Func a r    -> ppProto r [ppCheckArg ppBinding a]

ppName :: Binding r a -> Doc
ppName b = case b of
    Var r s     -> text s
    Func a r    -> ppName r 
    other       -> ppBinding other

ppProto :: Binding r a -> [Doc] -> Doc
ppProto r as = case r of
    v @ (Var r c) -> ppBinding v <> parens (commasep as)
    Func r' a'    -> ppProto r' (as ++ [ppCheckArg ppBinding a'])
    other         -> error "Invalid argument to function"

ppCheckArg :: (Binding r a -> Doc) -> Binding r a -> Doc
ppCheckArg f a = case a of
    Var r s         -> text "const" <+> f a
    Func r x        -> error "Cannot give function as function argument"
    FragCoord       -> error "Cannot give 'gl_FragCoord' as function argument"
    FragColor       -> error "Cannot give 'gl_FragColor' as function argument"

ppExpr :: Expr r a -> Doc
ppExpr e = case e of
    Float x         -> float x
    Int x           -> int x
    Bool x          -> text $ if x then "true" else "false"
    Vec r x         -> ppRep r <> parens (commasep $ map ppExpr (vecToList x))
    Val v           -> ppName v 
    App f a         -> ppRewrite f a
    other           -> error "Invalid expression"

ppRewrite :: Expr r a -> Expr a () -> Doc
ppRewrite f a = case f of
    Rewrite b r     -> if a == b then ppExpr r else ppExpr a 
    other           -> ppApply f [ppExpr a]

ppApply :: Expr r a -> [Doc] -> Doc
ppApply f as = case f of
    App g a'        -> ppApply g (as ++ [ppExpr a'])
    Call g          -> ppName g <> parens (commasep as)
    Prim s          -> text s <> parens (commasep as)
    Prim2 s         -> text s <> parens (commasep as)
    BinOp s         -> case as of [y,x] -> parens (x <+> text s <+> y)
    Rewrite b r     -> error "Rewrite term still present in AST"

ppDecl :: Decl r a -> Doc
ppDecl d = case d of
    Value b e       -> text "const" <+> ppBinding b <+> equals <+> ppExpr e
    Uniform b e     -> text "uniform" <+> ppBinding b <>
                        (case e of Nothing -> text ""
                                   Just x -> ppExpr x) <> semi
    Procedure b s   -> ppBinding b $+$ braceblock (ppStmt s)
    Function b e    -> ppBinding b $+$ braceblock (text "return" <+> ppExpr e <> semi)
   
ppStmt :: Stmt r -> Doc
ppStmt s = case s of
    Loc b e         -> ppBinding b <+> equals <+> ppExpr e <> semi
    Mut b e         -> ppName b <+> equals <+> ppExpr e <> semi
    Seq r s         -> ppStmt r $+$ ppStmt s
    If p i e        -> text "if" <> parens (ppExpr p)
                        <+> braceblock (ppStmt i)
                        $+$ text "else" <+> braceblock (ppStmt e)
    For v i p a s   -> text "for" <> parens (ppStmt (Loc v i) <> semi
                                            <+> ppExpr p <> semi
                                            <+> ppStmt (Mut v (App a (Val v))))
                                 <+> braceblock (ppStmt s)
    While p s       -> text "while" <> parens (ppExpr p) <+> braceblock (ppStmt s)
    Break           -> text "break" <> semi
    Cont            -> text "continue" <> semi
    Ret e           -> text "return" <+> ppExpr e <> semi
    Halt            -> text "return" <> semi
    Discard         -> text "discard" <> semi
