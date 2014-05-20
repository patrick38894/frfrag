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
          | Asgn String Expr
          | Sel (Sel Stmt)
          | Iter (Iter Stmt)
          | Block [Stmt]
          | CallProc String String [Expr]
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
                       printMain (fragMain frag)]

instance Show Frag where
    show = render . printFrag

eqSign = text "="
retStmt = text "return"
blockbraces d = lbrace $+$ nest 4 d $+$ rbrace
argList p es = parens (commasep (map p es))
commasep = cat . punctuate (text ", ")
semicolSep = sep . punctuate (text "; ")

printBinding (Bind t v) = sep [printType t, text v]
printUni (Uni b l) = (sep $ (text "uniform" <+> printBinding b)
                   : case l of (Just d) -> [eqSign, printLit d]
                               Nothing -> []) <> semi

printDecl d = case d of
    Val b e -> sep [printBinding b, eqSign, printExpr e] <> semi
    Func b args e -> sep [printBinding b <> argList printBinding args,
                         blockbraces (sep [retStmt, printExpr e] <> semi)]
    Proc b args stmts -> sep [printBinding b,
                             argList printBinding args,
                             blockbraces (sep $ map printStmt stmts)]
    GLProc b args stmts -> undefined


printStmt s = case s of
    DeclVar b e -> printBinding b <+> eqSign <+> printExpr e <> semi
    Asgn sn e -> text sn <+> eqSign <+> printExpr e <> semi
    Sel s -> printSel s
    Iter i -> printIter i
    Block stmts -> blockbraces (vcat (map printStmt stmts))
    CallProc sn pn es -> text sn <+> eqSign <+> text pn <> argList printExpr es <> semi
    NoOp -> empty
    Continue -> text "continue" <> semi
    Break -> text "break" <> semi
    Return e -> retStmt <+> printExpr e <> semi

printIter i = case i of
    For b init cond action block -> sep [text "for" <> parens 
                                        (semicolSep [printBinding b <+> eqSign <+> printExpr init,
                                                     printExpr cond, printForAction action]),
                                        printStmt block]
    While cond block -> sep [text "while" <> parens (printExpr cond), printStmt block]
    DoWhile block cond -> sep [text "do" <+> printStmt block, 
                              text "while" <> parens (printExpr cond)]

printForAction a = case a of
    Asgn sn e -> text sn <+> eqSign <+> printExpr e

printSel s = case s of
    IfElse cond ifCase elseCase -> sep [text "if" <> parens (printExpr cond),
                                       printStmt ifCase,
                                       case elseCase of NoOp -> empty
                                                        x -> sep [text "else", printStmt x]]
    Switch expr cases -> undefined


printExpr e = case e of
    FragCoord -> text "gl_FragCoord"
    Lit l -> printLit l
    Con t es -> printType t <> argList printExpr es
    LookFunc s es -> printFunc s es
    LookVal s -> text s
    Swizzle e s -> printExpr e <> text ('.':s)
    x -> printPrim x

printFunc s es = text s <> argList printExpr es

printPrim x = case x of
    Add e1 e2 -> printAddOp "+" e1 e2
    Sub e1 e2 -> printAddOp "-" e1 e2
    Mul e1 e2 -> printMulOp "*" e1 e2
    Div e1 e2 -> printMulOp "/" e1 e2
    LessThan e1 e2 -> printAddOp "<" e1 e2
    Dot e1 e2 -> printFunc "dot" [e1, e2]
    -- Lots of missing cases
    x -> error (show x)

printAddOp op e1 e2 = parens $ printExpr e1 <+> text op <+> printExpr e2
printMulOp op e1 e2 = printExpr e1 <+> text op <+> printExpr e2

printGLStmt stmt = case stmt of
    Discard -> text "discard" <> semi
    GLSel s -> undefined
    GLIter i -> undefined
    GLBlock b -> blockbraces (sep (map printGLStmt b))
    GLGet s v -> text s <+> eqSign <+> printGLVar v <> semi
    GLAsgn v e -> printGLVar v <+> eqSign <+> printExpr e <> semi

printGLVar v = case v of
    GLFragColor -> text "gl_FragColor"

printMain stmt = sep [text "void main()", blockbraces (printGLStmt stmt)]
