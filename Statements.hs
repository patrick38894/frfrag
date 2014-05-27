{-# Language FlexibleContexts, GADTs, KindSignatures, UndecidableInstances #-}
module Statements where
import Text.PrettyPrint.HughesPJ
import Expressions
import HigherOrder

data Decl :: * -> * where
    Value :: Binding t -> Expr t -> Decl t
    Uniform :: Binding t -> Maybe (Expr t) -> Decl t
    Procedure :: Binding (a -> r) -> Stmt -> Decl (a -> r)
    Function :: (Pretty a, Wrap Expr a, Wrap Rep a) => Binding (a -> r) -> Expr a -> Decl (a -> r)


data Stmt where
    Block :: [Stmt] -> Stmt
    DecVar :: (Pretty t, Wrap Expr t, Wrap Rep t) => Binding t -> Expr t -> Stmt
    Mutate :: (Pretty t, Wrap Expr t, Wrap Rep t) => Binding t -> Expr t -> Stmt
    Switch :: Expr Int -> [(Int, Stmt)] -> Stmt
    IfElse :: Expr Bool -> Stmt -> Stmt -> Stmt
    For :: Binding Int -> Expr Int -> Expr (Int -> Bool) -> Expr (Int -> Int) -> Stmt -> Stmt
    While :: Expr Bool -> Stmt -> Stmt
    Break :: Stmt
    Continue :: Stmt
    Return :: (Pretty a, Wrap Expr a, Wrap Rep a) => Expr a -> Stmt
    Terminate :: Stmt
    Discard :: Stmt

------------------------------------------------------------------------------

instance Extract Decl where 
  extract e = case e of
    Value name expr -> undefined
    Uniform bind def -> undefined
    --Procedure proc stmt -> undefined
    --Function func expr -> undefined

instance (Wrap Expr t, Wrap Rep t, Pretty t) => Pretty (Decl t) where
  pp decl = case decl of
    Value b e       -> pp "const" <+> pp b <+> equals <+> pp e <> semi
    Uniform b e     -> pp b <> (case e of
                        Just x -> pp "" <+> equals <+> pp x
                        Nothing -> empty ) <> semi  
    Procedure b stmt -> case stmt of 
                             Block x -> pp b $+$ (pp stmt)
                             other -> pp b $+$ braceblock (pp stmt)
    Function b expr -> pp b $+$ braceblock (pp "return" 
                                <+> pp expr <> semi)
   
instance Pretty Stmt where
  pp s = case s of
    Block xs -> braceblock . vcat $ map pp xs
    DecVar b e -> pp b <+> equals <+> pp e <> semi
    Mutate b e -> (case b of
                    Var r nm -> pp nm
                    FragColor -> pp FragColor
                    Swiz r s -> pp  (Swiz r s)
                    other -> error $ "Cannot mutate binding '" ++ show other ++ "'"
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


