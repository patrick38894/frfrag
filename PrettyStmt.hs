{-# Language FlexibleContexts, GADTs, UndecidableInstances #-}
module PrettyStmt where
import Text.PrettyPrint.HughesPJ
import Pretty
import Expressions
import Statements
import HigherOrder

instance (Wrap Expr t, Wrap Rep t, Pretty t) => Pretty (Decl t) where
  pp decl = case decl of
    Value b e       -> pp b <+> equals <+> pp e
    Uniform b e     -> pp b <> (case e of
                        Just x -> empty <+> equals <+> pp x
                        Nothing -> empty ) <> semi  
    Procedure b stmt -> pp b $+$ braceblock (pp stmt)
    Function b expr -> pp b $+$ braceblock (pp "return" <+> pp expr <> semi)
   
instance Pretty Stmt where
  pp s = case s of
    Block xs -> braceblock . sep $ map pp xs
    DecVar b e -> pp b <+> equals <+> pp e <> semi
    Mutate b e -> (case b of
                    Var r nm -> pp nm
                    FragColor -> pp FragColor
                    other -> error $ "Cannot mutate binding" ++ show other
                    ) <+> equals <+> pp e <> semi
    Switch p cs -> pp "switch" <> parens (pp p) $+$ braceblock (ppcases cs)
    IfElse p i e -> pp "if" <> parens (pp p) $+$ braceblock (pp i) 
                  $+$ pp "else" $+$ braceblock (pp e)
    For (v) i p t s -> pp "for" 
                <> parens (sep [pp v <+> equals <+> pp i <> semi,
                                pp (App p (Val v)), pp (Mutate v (App t (Val v)))]) 
                $+$ braceblock (pp s)
    While p s -> pp "while" <> parens (pp p) $+$ braceblock (pp s)
    Break -> pp "break;"
    Continue -> pp "continue;"
    Return a -> pp "return" <+> pp a <> semi
    Terminate -> pp "return;"
    Discard -> pp "discard;"
    
ppcases :: [(Int, Stmt)] -> Doc
ppcases ((i,s):cs) = pp "case" <> parens (pp i) $+$ braceblock (pp s) $+$ ppcases cs

instance Pretty (Decl a) => Show (Decl a) where show = render . pp
instance Show Stmt where show = render . pp 


