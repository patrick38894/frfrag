module Primitive where
import CoreLanguage

true = Bool True
false = Bool False

eqE, ltE, lteE, gtE, gteE :: Expr a -> Expr a -> Expr Bool
eqE = BinOp "=="
ltE = BinOp "<"
lteE = BinOp "<="
gtE = BinOp ">"
gteE = BinOp ">="

orE, andE :: Expr Bool -> Expr Bool -> Expr Bool
orE = BinOp "||"
andE = BinOp "&&"
notE :: Expr (Bool -> Bool)
notE = let v = Var "varNOT" BoolT in Lam v (BinOp "&& !" true $ Val v)

addE, subE, mulE :: Num a => Expr a -> Expr a -> Expr a
addE = BinOp "+" 
subE = BinOp "-"
mulE = BinOp "*"

absE, sgnE :: Num a => Expr a -> Expr a
absE = Prim "abs"
sgnE = Prim "sign"

divE, modE :: Expr Int -> Expr Int -> Expr Int
divE = BinOp "/"
modE = BinOp "%"

fdivE :: Fractional a => Expr a -> Expr a -> Expr a
fdivE = BinOp "/"

sqrtE, expE, logE, sinE, cosE, tanE :: Floating a => Expr a -> Expr a
sqrtE = Prim "sqrt"
expE = Prim "exp"
logE = Prim "log"
sinE = Prim "sin"
cosE = Prim "cos"
tanE = Prim "tan"
sinhE = Prim "sinh"
coshE = Prim "cosh"
asinE = Prim "asin"
acosE = Prim "acos"
atanE = Prim "atan"
asinhE = Prim "asinh"
acoshE = Prim "acosh"
atanhE = Prim "atanh"

atan2E, powE :: Floating a => Expr a -> Expr a -> Expr a
atan2E = Prim2 "atan2"
powE = Prim2 "pow"

dotE :: Num a => Expr (VecN a) -> Expr (VecN a) -> Expr a
dotE = Prim2 "dot"

-- Componentwise matrix multiplication
compMultE :: Num a => Expr (MatN a) -> Expr (MatN a) -> Expr (MatN a)
compMultE = Prim2 "matrixCompMult"
