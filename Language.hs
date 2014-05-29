{-# Language GADTs, KindSignatures #-}
module Language where
------------------------------------------------------------------------------
data N = N2 | N3 | N4
data VecN :: (* -> *) where
    Vec2 :: a -> a -> VecN a
    Vec3 :: a -> a -> a -> VecN a
    Vec4 :: a -> a -> a -> a -> VecN a
intN :: N -> Int
intN n = case n of N2 -> 2; N3 -> 3; N4 -> 4

vecToList :: VecN a -> [a]
vecToList v = case v of
    Vec2 a b -> [a,b]
    Vec3 a b c -> [a,b,c]
    Vec4 a b c d -> [a,b,c,d]

------------------------------------------------------------------------------
data Rep :: * -> * -> * where
    VoidT :: Rep () ()
    BoolT :: Rep () Bool
    IntT :: Rep () Int
    FloatT :: Rep () Float
    VecT :: Rep () a -> N -> Rep () (VecN a)
    FuncT :: Rep () a -> Rep a' r -> Rep a (Rep a' r)

------------------------------------------------------------------------------
data Binding :: * -> * -> * where
    Void :: Binding () ()
    FragCoord :: Binding () (VecN Float)
    FragColor :: Binding () (VecN Float)
    Var :: Rep () t -> String -> Binding () t
    Func :: Binding () a -> Binding a' r -> Binding a r

------------------------------------------------------------------------------
data Expr :: * -> * -> * where
    Float :: Float -> Expr () Float
    Bool :: Bool -> Expr () Bool
    Int :: Int -> Expr () Int
    Vec :: Rep () r -> VecN (Expr () r) -> Expr () (VecN r)
    Val :: Binding () r -> Expr () r
    Call :: Binding a r -> Expr a r
    Prim :: String -> Expr a r
    Prim2 :: String -> Expr a (Expr a' r)
    BinOp :: String -> Expr a (Expr a' r)
    Rewrite :: Expr a a' -> Expr a' r' -> Expr a r'
    Sym :: Rep a r -> Int -> Expr a r
    App :: Expr a' r -> Expr a a' -> Expr a r

------------------------------------------------------------------------------

