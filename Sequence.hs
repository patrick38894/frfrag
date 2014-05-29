module Sequence where
import Monad
import Language

data Loop = Sequence Int Int Int
          | Map Expr Loop
          | Zip Loop Loop
          | Fold Expr Expr Loop

seqLoop :: Loop -> Interpret Stmt
seqLoop = undefined
