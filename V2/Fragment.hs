{-# Language GADTs #-}

module Fragment where

import Data.Char
import Text.PrettyPrint.HughesPJ

import V2.Rep
-- import V2.Bind
-- import V2.Expr
-- import V2.Decl
-- import V2.Stmt
-- import V2.Prim

data Frag = Frag {uniforms        :: [Uniform], 
                  declarations    :: [Decl], 
                  fragMain        :: GLStmt} 
