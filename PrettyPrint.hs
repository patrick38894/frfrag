{-# Language FlexibleInstances, GADTs #-}
module PrettyPrint where
import Text.PrettyPrint.HughesPJ hiding (float, int)
import qualified Text.PrettyPrint.HughesPJ as PP (float, int)
import Language
import Vector
import Monad
import Region
import Control.Monad.Reader
import Data.Map (toAscList)

commasep :: [Doc] -> Doc
commasep = sep . punctuate comma 
braceblock :: Doc -> Doc
braceblock d  = lbrace $+$ nest 4 d $+$ rbrace

ppN :: N -> Doc
ppN = text . show . intN

ppVecN :: Show a => VecN a -> Doc
ppVecN = commasep . map (text . show) . vecToList

ppRep :: Rep -> Doc
ppRep r = case r of
    VoidT       -> text "void"
    BoolT       -> text "bool"
    IntT        -> text "int"
    FloatT      -> text "float"
    VecT r n    -> text (getInitial r) <> text "vec" <> ppN n

getInitial :: Rep -> String
getInitial r = case r of
    BoolT -> "b"
    IntT -> "i"
    FloatT -> ""

ppBinding :: Bind -> Doc
ppBinding b = case b of
    Void        -> empty
    FragCoord   -> text "gl_FragCoord"
    FragColor   -> text "gl_FragColor"
    Var r s     -> ppRep r <+> text s 
    Func r as    -> ppBinding r <> parens (commasep $ map ppBinding as)

ppName :: Bind -> Doc
ppName b = case b of
    Var r s     -> text s
    Func r a    -> ppName r 
    other       -> ppBinding other

ppCheckArg :: (Bind -> Doc) -> Bind -> Doc
ppCheckArg f a = case a of
    Var r s         -> text "const" <+> f a
    Func r x        -> error "Cannot give function as function argument"
    FragCoord       -> error "Cannot give 'gl_FragCoord' as function argument"
    FragColor       -> error "Cannot give 'gl_FragColor' as function argument"

ppExpr :: Expr -> Doc
ppExpr e = case e of
    Float x         -> PP.float x
    Int x           -> PP.int x
    Bool x          -> text $ if x then "true" else "false"
    Vec r x         -> ppRep r <> parens (commasep $ map ppExpr (vecToList x))
    Val v           -> ppName v 
    Call b as       -> ppName b <> parens (commasep $ map ppExpr as)
    Prim s a        -> text s <> parens (ppExpr a)
    Prim2 s a b     -> text s <> parens (commasep $ map ppExpr [a,b])
    BinOp s a b     -> parens $ ppExpr a <+> text s <+> ppExpr b

ppDecl :: Decl -> Doc
ppDecl d = case d of
    Value b e       -> text "const" <+> ppBinding b <+> equals <+> ppExpr e
    Uniform b e     -> text "uniform" <+> ppBinding b <>
                        (case e of Nothing -> empty
                                   Just x -> text "" <+> equals <+> ppExpr x) <> semi
    Procedure b s   -> ppBinding b $+$ braceblock (ppStmt s)
    Function b e    -> ppBinding b $+$ braceblock (text "return" <+> ppExpr e <> semi)
   
ppStmt :: Stmt -> Doc
ppStmt s = case s of
    Loc b e         -> ppBinding b <+> equals <+> ppExpr e <> semi
    Mut b e         -> ppName b <+> equals <+> ppExpr e <> semi
    Seq rs          -> vcat $ map ppStmt rs
    If p i e        -> text "if" <> parens (ppExpr p)
                        <+> braceblock (ppStmt i)
                        $+$ text "else" <+> braceblock (ppStmt e)
    For v i p a s   -> text "for" <> parens (ppStmt (Loc v i) <> semi
                                          <+> ppExpr p <> semi
                                          <+> ppStmt (Mut v a))
                               <+> braceblock (ppStmt s)
    While p s       -> text "while" <> parens (ppExpr p) <+> braceblock (ppStmt s)
    Break           -> text "break" <> semi
    Cont            -> text "continue" <> semi
    Ret e           -> text "return" <+> ppExpr e <> semi
    Halt            -> text "return" <> semi
    Discard         -> text "discard" <> semi
    NoOp            -> empty

ppFrag :: Fragment -> Doc
ppFrag (Fragment (e, m ,r)) = ppEnv e $+$ ppMain m r

ppEnv :: Env -> Doc
ppEnv = vcat . map (ppDecl . snd) . toAscList
ppMain :: Stmt -> Region -> Doc
ppMain m r = ppStmt $ shadeRegion m r

instance Show Fragment where show = render . ppFrag
instance Show (Interpret a) where show = show . interpret
