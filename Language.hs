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
         | FuncT Rep [Rep]
         deriving (Eq, Ord, Show)
------------------------------------------------------------------------------
data Bind = Void
             | FragCoord
             | FragColor
             | Var Rep String
             | Func Bind [Bind]
             deriving (Eq, Ord, Show)
------------------------------------------------------------------------------
data Expr = Float Float
          | Bool Bool
          | Int Int
          | Vec Rep (VecN Expr)
          | Val Bind
          | Call Bind
          | Prim String
          | Prim2 String
          | BinOp String
          | App Expr [Expr]
          | Lam Int Rep Expr
          | Sym Int Rep
          deriving (Eq, Show)
------------------------------------------------------------------------------
data Decl = Value Bind Expr
          | Uniform Bind (Maybe Expr)
          | Procedure Bind Stmt
          | Function Bind [Expr]
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

prim = App . Prim
prim2 = App . Prim2
binOp = App . BinOp
