{-# Language ExistentialQuantification, 
             FlexibleContexts, 
             FlexibleInstances, 
             GADTs, 
             KindSignatures,
             MultiParamTypeClasses,
             RankNTypes,
             UndecidableInstances #-}

import GHC.Exts

data Z = Z
data S t = S t
type One = S Z
type Two = S (S Z)
type Three = S (S (S Z))
type Four = S (S (S (S Z)))

type N1 = Dim One
type N2 = Dim Two
type N3 = Dim Three
type N4 = Dim Four

n1 = S Z
n2 = S . S $ Z
n3 = S . S . S $ Z
n4 = S . S . S . S $ Z

class IsNat n where
    nat :: n -> Int
    fromInt :: Int -> n

instance IsNat Z where
    nat Z = 0
    fromInt 0 = Z

instance IsNat t => IsNat (S t) where
    nat (S t) = 1 + nat t
    fromInt n =  S (fromInt (n - 1))

class IsDim a where
    dims :: a -> [Int]
    fromDims :: [Int] -> a

data Dim :: * -> * where 
    MDim :: IsDim n => n -> Dim n
    Dim :: IsNat n => n -> Dim n

instance IsNat n => IsDim (Dim n) where
    dims (Dim n) = [nat n]
    fromDims [n] = Dim (fromInt n)

instance (IsDim n, IsDim m) => IsDim (n, m) where
    dims (n, m) = dims n ++ dims m
    fromDims [n, m] = (fromDims [n], fromDims [m])

class GenType a n where getRep :: IsDim n => a -> Rep a n

instance GenType Bool N1 where getRep a = Bool
instance GenType Int N1 where getRep a = Int
instance GenType Float N1 where getRep a = Float

instance (GenType a N1, IsNat n) => GenType [a] n where
    getRep xs = let n = fromDims [length xs]
                    t = getRep (head xs)
                in Vec n t

instance (GenType a N1, IsNat n, IsNat m) => GenType [[a]] (n,m) where
    getRep xs = let n = fromDims [length xs]
                    m = fromDims [(the . map length $ xs)]
                    t = getRep (head (head xs))
                in Mat n m t

scalarRep :: Rep [a] n -> Rep a N1
scalarRep = undefined

data Rep :: * -> * -> * where
    Bool :: Rep Bool N1
    Int :: Rep Int N1
    Float :: Rep Float N1
    Vec ::  Dim n -> Rep a N1 -> Rep [a] n
    Mat :: Dim n -> Dim m -> Rep a N1 -> Rep [[a]] (n, m)
    Swiz :: GenType b n => Rep [a] n -> String -> Rep b m
    Gen :: GenType a n => Rep a n

x :: (IsDim n, IsNat n, GenType a N1, GenType [a] n) => Rep [a] n -> Rep a N1
x v = case v of Vec n xs -> undefined
