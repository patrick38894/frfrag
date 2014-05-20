module Fragment where
import Literals
import Data.Char
import Text.PrettyPrint.HughesPJ

data Frag = Frag {uniforms        :: [Uniform], 
                  declarations    :: [Decl], 
                  fragMain        :: GLStmt} 

data Binding = Bind Type String deriving Show
data Uniform = Uni Binding (Maybe Lit)

data Decl = Val Binding Expr
          | Func Binding [Binding] Expr                    
          | Proc Binding [Binding] [Stmt]
          | GLProc Binding [Binding] [GLStmt]
          deriving Show

data Stmt = DeclVar Binding Expr
          | DeclVal Binding Expr
          | Asgn String Expr
          | Sel (Sel Stmt)
          | Iter (Iter Stmt)
          | Block [Stmt]
          | CallProc Binding Binding [Expr]
          | NoOp
          | Continue
          | Break
          | Return Expr
          deriving Show

data Sel a = IfElse Expr a a
           | Switch Expr [(Maybe Int, a)]
           deriving Show

data Iter a = For Binding Expr Expr a a
            | While Expr a
            | DoWhile a Expr
            deriving Show

data Expr = FragCoord
          | Lit Lit
          | Con Type [Expr]
          | LookFunc String [Expr]
          | LookVal String
          | Swizzle Expr String
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | MatCompMul Expr Expr
          | Div Expr Expr
          | Eq Expr Expr
          | LessThan Expr Expr
          | GreaThan Expr Expr
          | LessThanEq Expr Expr
          | GreaThanEq Expr Expr
          | Or Expr Expr
          | And Expr Expr
          | Sqrt Expr
          | InvSqrt Expr 
          | Pow Expr Expr
          | Exp Expr 
          | Log Expr
          | Exp2 Expr
          | Log2 Expr
          | Sin Expr
          | Cos Expr
          | Tan Expr
          | Dot Expr Expr
          deriving Show

data GLStmt = Scoped Stmt
            | Discard 
            | GLSel (Sel [GLStmt])
            | GLIter (Iter GLStmt)
            | GLBlock [GLStmt]
            | GLGet String GLVar
            | GLAsgn GLVar Expr 
            deriving Show

data GLVar = GLFragColor deriving Show

------------------------------------------------------------------------------
-- Pretty printing and show instances ----------------------------------------

printFrag frag = vcat [ vcat $ map printUni (uniforms frag),
                       vcat $ map printDecl (declarations frag),
                       printGLStmt (fragMain frag)]

instance Show Frag where
    show = render . printFrag

eqSign = text "="
retStmt = text "return"
blockbraces d = text "{" $+$ nest 4 d $+$ text "}"

argList p es = parens (commasep (map p es))
commasep = cat . punctuate (text ", ")

printBinding (Bind t v) = sep [printType t, text v]
printUni (Uni b l) = sep $ printBinding b
                   : case l of (Just d) -> [eqSign, printLit d]
                               Nothing -> []

printDecl d = case d of
    Val b e -> sep [printBinding b, eqSign, printExpr e]
    Func b args e -> sep [printBinding b <> argList printBinding args,
                         blockbraces (sep [retStmt, printExpr e])]
    Proc b args stmts -> sep [printBinding b,
                             argList printBinding args,
                             blockbraces (sep $ map printStmt stmts)]
    GLProc b args stmts -> undefined


printStmt = text . show

printGLStmt = text . show 

printLit = text . show
printType = text . (\(x:xs) -> toLower x : xs) . show

printExpr e = case e of
    FragCoord -> text "glFragCoord"
    Lit l -> printLit l
    Con t es -> printType t <> argList printExpr es
    LookFunc s es -> text s <> argList printExpr es
    LookVal s -> text s
    Swizzle e s -> printExpr e <> text ('.':s)
    Add e1 e2 -> printExpr e1 <+> text "+" <+> printExpr e2
    Sub e1 e2 -> printExpr e1 <+> text "-" <+> printExpr e2
    Mul e1 e2 -> printExpr e1 <+> text "*" <+> printExpr e2
    Div e1 e2 -> printExpr e1 <+> text "/" <+> printExpr e2
    x -> error (show x)
