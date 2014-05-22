{-# Language ExistentialQuantification, 
             FlexibleContexts, 
             FlexibleInstances, 
             GADTs, 
             KindSignatures,
             MultiParamTypeClasses,
             RankNTypes,
             UndecidableInstances #-}

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

class IsDim a where
    dims :: a -> [Int]
    fromDims :: [Int] -> a

data Dim :: * -> * where Dim :: IsDim n => n -> Dim n

instance IsDim n => IsDim (Dim n) where
    dims (Dim n) = dims n
    fromDims n = Dim (fromDims n)

instance IsDim One where
    dims = const [1]
    fromDims [1] = S Z

instance IsDim Two where 
    dims = const [2]
    fromDims [2] = S (S Z)

instance IsDim Three where
    dims = const [3]
    fromDims [3] = S (S (S Z))

instance IsDim Four where 
    dims = const [4]
    fromDims [4] = n4

instance (IsDim n, IsDim m) => IsDim (n, m) where
    dims (n, m) = dims n ++ dims m
    fromDims [n, m] = (fromDims [n], fromDims [m])

class GenType a n where getRep :: IsDim n => a -> Rep a n

instance GenType Bool N1 where getRep a = Bool
instance GenType Int N1 where getRep a = Int
instance GenType Float N1 where getRep a = Float

instance GenType a N1 => GenType [a] n where
    getRep xs = let l = fromDims [length xs]
                    t = getRep (head xs)
                in Vec l t

scalarRep :: Rep [a] n -> Rep a N1
scalarRep = undefined

data Rep :: * -> * -> * where
    Bool :: Rep Bool N1
    Int :: Rep Int N1
    Float :: Rep Float N1
    Vec :: Dim n -> Rep a N1 -> Rep [a] n
    Mat :: Dim n -> Dim m -> Rep a N1 -> Rep [[a]] (n, m)
    Swiz :: GenType b n => Rep [a] n -> String -> Rep b m
    Gen :: GenType a n => Rep a n

x :: (IsDim n, GenType a N1, GenType [a] n) => Rep [a] n -> Rep a N1
x v = case v of Vec n xs -> undefined
