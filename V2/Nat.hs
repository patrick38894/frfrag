{-# Language ExistentialQuantification, 
             FlexibleContexts, 
             FlexibleInstances, 
             GADTs, 
             KindSignatures,
             MultiParamTypeClasses,
             RankNTypes,
             UndecidableInstances #-}

data One = One
data Two = Two
data Three = Three
data Four = Four

type N1 = Dim One
type N2 = Dim Two
type N3 = Dim Three
type N4 = Dim Four

n1 = Dim One :: N1
n2 = Dim Two :: N2
n3 = Dim Three :: N3
n4 = Dim Four :: N4

class IsDim a where
    dims :: [a] -> [Int]
    fromDims :: [Int] -> [a]

data Dim :: * -> * where
    Dim :: IsDim n => n -> Dim n

instance IsDim One where
    dims = const [1]
    fromDims [1] = [One]

instance IsDim n => IsDim (Dim n) where
    dims [Dim n] = dims [n]
    fromDims (i:is) = fmap Dim (fromDims [i]) ++ fromDims is

instance IsDim Two where 
    dims = const [2]
    fromDims [2] = [Two]

instance IsDim Three where
    dims = const [3]
    fromDims [3] = [Three] 

instance IsDim Four where 
    dims = const [4]
    fromDims [4] = [Four]

class GenType a n where getRep :: IsDim n => a -> Rep a n

instance GenType Bool N1 where getRep a = Bool
instance GenType Int N1 where getRep a = Int
instance GenType Float N1 where getRep a = Float

instance GenType a N1 => GenType [a] n where
    getRep xs = let [l] = fromDims [length xs]
                    t = getRep (head xs)
                in Vec l t

data Rep :: * -> * -> * where
    Bool :: Rep Bool N1
    Int :: Rep Int N1
    Float :: Rep Float N1
    Vec :: Dim n -> Rep a N1 -> Rep [a] n
    Mat :: Dim n -> Dim m -> Rep a N1 -> Rep [[a]] (n, m)
    Swiz :: GenType b n => Rep [a] n -> String -> Rep b m
    Gen :: GenType a n => Rep a n

x :: (IsDim n, GenType a N1, GenType [a] n) => Rep [a] n -> Rep a N1
x v = undefined 
