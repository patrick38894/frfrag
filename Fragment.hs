module Fragment where
import Literals

data Frag = Frag {uniforms        :: [Uniform], 
                  declarations    :: [Decl], 
                  fragMain        :: GLStmt} 
            deriving Show

data Binding = Bind Type String deriving Show
data Uniform = Uni Binding (Maybe Lit) deriving Show

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


