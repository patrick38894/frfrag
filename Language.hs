{-# Language
        FlexibleInstances,
        GADTs,
        KindSignatures,
        TypeSynonymInstances #-}

module Language where
import Vector
------------------------------------------------------------------------------
data Rep = VoidT
         | BoolT
         | IntT
         | FloatT
         | VecT Rep N
         deriving (Eq, Ord, Show)
------------------------------------------------------------------------------
data Bind = Void
          | FragCoord
          | FragColor
          | Var Rep String
          | Func Bind [Bind]
          | Swiz Bind String 
          deriving (Eq, Ord, Show)
------------------------------------------------------------------------------
data Expr = Float Float
          | Bool Bool
          | Int Int
          | Vec Rep (VecN Expr)
          | Val Bind
          | Call Bind [Expr]
          | Prim String Expr
          | Prim2 String Expr Expr
          | BinOp String Expr Expr
          deriving (Eq, Show)
------------------------------------------------------------------------------
data Decl = Value Bind Expr
          | Uniform Bind (Maybe Expr)
          | Procedure Bind Stmt
          deriving (Eq, Show)
------------------------------------------------------------------------------
data Stmt = Loc Bind Expr
          | Mut Bind Expr
          | Seq [Stmt]
          | If Expr Stmt Stmt
          | Case [(Int, Stmt)]
          | For Bind Expr Expr Expr Stmt
          | While Expr Stmt
          | Break
          | Cont
          | Ret Expr
          | Halt
          | Discard
          | NoOp
          deriving (Eq, Show)
------------------------------------------------------------------------------
int = IntT
intp = Var int
float = FloatT
floatp = Var float
bool = BoolT
boolp = Var bool
true = Bool True
false = Bool False
vec2t = VecT FloatT N2
vec2p = Var vec2t
vec2e = Val . vec2p
vec3t = VecT FloatT N3
vec3p = Var vec3t
vec3e = Val . vec3p
vec4t = VecT FloatT N4
vec4p = Var vec4t
vec4e = Val . vec4p
vec :: [Float] -> Expr
vec xs = Vec (VecT FloatT (fromInt (length xs))) (vecFromList (map Float xs)) 

fragCoord = Val FragCoord
