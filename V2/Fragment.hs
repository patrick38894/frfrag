{-# Language ExistentialQuantification, 
             FlexibleInstances,
             GADTs,
             KindSignatures,
             StandaloneDeriving,
             TypeSynonymInstances #-}

module Fragment where

import Data.Char
import Text.PrettyPrint.HughesPJ
import Control.Monad.State
import GHC.Exts

{-data Frag = Frag {uniforms        :: [Uniform], 
                  declarations    :: [Decl], 
                  fragMain        :: GLStmt} 

-}

------------------------------------------------------------------------------
-- Representations of types --------------------------------------------------

data SmallNat = N2 | N3 | N4 deriving (Eq, Enum, Ord)

data Rep :: * -> * where
    Bool :: Rep Bool
    Int :: Rep Int
    Float :: Rep Float
    Vec :: SmallNat -> Rep a -> Rep [a]
    Mat :: SmallNat -> SmallNat -> Rep a -> Rep [[a]]

class GetRep a where
    getRep :: a -> Rep a

instance GetRep Int where getRep = const Int
instance GetRep Float where getRep = const Float
instance GetRep Bool where getRep = const Bool

getVecRep :: GetRep a => [a] -> Rep [a]
getMatRep :: GetRep a => [[a]] -> Rep [[a]]
getVecRep xs = Vec (getDimEnum $ length xs) (getRep (head xs))
getMatRep xs = Mat (getDimEnum $ length xs) 
                   (getDimEnum . the $ map length xs) (getRep (head (head xs)))
getDimEnum :: Int -> SmallNat
getDimEnum n = toEnum (n-1)

-- Vector type synonyms are provided for convenience.
vec2 = Vec N2 Float
vec3 = Vec N3 Float
vec4 = Vec N4 Float
ivec2 = Vec N2 Int
ivec3 = Vec N3 Int
ivec4 = Vec N4 Int
bvec2 = Vec N2 Bool
bvec3 = Vec N3 Bool
bvec4 = Vec N4 Bool

------------------------------------------------------------------------------
-- Expressions ---------------------------------------------------------------

data Exp :: * -> * where
    BoolL :: Bool -> Exp Bool
    FloatL :: Float -> Exp Float
    IntL :: Int -> Exp Int
    VecE :: (GetRep t, PP t) => [t] -> Exp [t]
    MatE :: (GetRep t, PP t) => [[t]] -> Exp [[t]]
    VarE :: PP t => Binding t -> Exp t
    CallE :: PP r =>  Binding r -> Args -> Exp (Args -> r)

data Binding t = Bind String (Rep t)

-- Heterogenous, variable length argument lists.    
data Args = forall a. PP a => Arg (Binding a) Args | NoMoreArgs

------------------------------------------------------------------------------
-- Type and structure checking -----------------------------------------------

-- Execution environment, used to keep the GLSL output sane in the printer ---
data Env where
    Empty :: Env
    Extend :: String -> Rep t -> t -> Env -> Env

type Check = State Env

-- Starting environment for specialized GL statements,
-- containing "unsafe" GL operations.
gl_IO_env = undefined

------------------------------------------------------------------------------
-- Pretty printing -----------------------------------------------------------

instance Show SmallNat where show = render . pp
instance Show (Rep a) where show = render . pp

class PP x where
    pp :: x -> Doc

instance PP String where
    pp = text

instance PP SmallNat where
    pp n = pp . show $ case n of N2 -> 2; N3 -> 3; N4 -> 4

instance PP (Rep a) where
    pp r = case r of
        Bool        -> pp "bool"
        Float       -> pp "float"
        Int         -> pp "int"
        Vec n r     -> repInit r <> pp "vec" <> pp n
        Mat n m r   -> repInit r <> pp "mat" <> pp n 
                    <> if m == n then empty else pp "x" <> pp m
        where repInit r = pp $ case r of 
                                    Bool -> "b"
                                    Int -> "i"
                                    Float -> ""
                                    x -> error errNonScalar

instance PP (Exp a) where
    pp expr = case expr of
        BoolL b     -> pp (if b then "true" else "false")
        FloatL n    -> pp (show n)
        IntL n      -> pp (show n)
        VecE ts     -> pp (getVecRep ts) <> parens (arglist ts)
        MatE ts     -> pp (getMatRep ts) <> (parens . arglist . concat) ts
        VarE (Bind s r) -> pp s
        CallE (Bind s r) a -> pp s <> (parens . pp) a

instance PP (Binding t) where
    pp (Bind s r) = pp r <+> pp s

instance PP Args where
    pp args = case args of
        Arg x NoMoreArgs -> pp x
        Arg x a -> pp x <> comma <+> pp a

arglist :: PP x => [x] -> Doc
arglist = sep . punctuate comma . map pp


------------------------------------------------------------------------------
-- Error messages ------------------------------------------------------------
errNonScalar = "Non-scalar type as vector or matrix element."

