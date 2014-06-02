{-# Language DeriveFunctor #-}
module Print where
import Text.PrettyPrint.HughesPJ hiding (int, float)
import qualified Text.PrettyPrint.HughesPJ as PP
import Vector
import Decl
import Language

newtype PPExpr a = Expr { pp :: Int -> Doc } deriving Functor

instance Expr PPExpr where
    void            = Expr $ const empty
    bool x          = Expr $ const $ text $ if x then "true" else "false"
    int x           = Expr $ const (PP.int x)
    float x         = Expr $ const (PP.float x)
    vec x           = Expr $ \h -> sep $ punctuate comma $ toList $ fmap (\n -> pp n h) x
    mulOp s x y     = Expr $ \h -> pp x h <+> text s <+> pp y h
    addOp s x y     = Expr $ \h -> parens (pp x h <+> text s <+> pp y h)
    compOp s x y    = Expr $ \h -> pp x h <+> text s <+> pp y h
    prim s x        = Expr $ \h -> text s <> parens (pp x h)
    prim2 s x y     = Expr $ \h -> text s <> parens (pp x h <> comma <+> pp y h)

data PPDecl a = PPDecl { firstPP :: Int -> Doc, otherPP :: BuildProg a }
              | EndPP
              deriving Functor
printProg :: BuildProg a -> PPDecl a
printProg = undefined

