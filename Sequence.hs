module Sequence where
import Fragment
import Language

data Loop = Sequence Int Int Int
          | Map Expr Loop
          | Zip Loop Loop
          | Fold Expr Expr Loop
