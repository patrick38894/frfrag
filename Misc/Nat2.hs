{-# Language DataKinds, 
             GADTs,
             KindSignatures,
             PolyKinds,
             RankNTypes,
             StandaloneDeriving #-}

data Nat = Z | S Nat deriving Show
n1 = S Z
n2 = S n1
n3 = S n2
n4 = S n3


infixr :-
data Vec :: * -> Nat -> * where
    Nil :: Vec a Z
    (:-) :: a -> Vec a n -> Vec a (S n)

instance Show a => Show (Vec a n) where
    show Nil = "Nil"
    show (a :- bs) = unwords [show a, ":-", show bs]

type Sca a = Vec a (S Z)
type Vec2 a = Vec a (S (S Z))
type Vec3 a = Vec a (S (S (S Z)))
type Vec4 a = Vec a (S (S (S (S Z))))

type ScaP a = forall n . Vec a (S n)
type Vec2P a n = Vec a (S (S n))
type Vec3P a = forall n . Vec a (S (S (S n)))
type Vec4P a = forall n . Vec a (S (S (S n)))

x :: ScaP a -> a
y :: Vec2P a n -> a
z :: Vec3P a -> a
w :: Vec4P a -> a

x (a :- bs) = a
y (a :- b :- cs) = b
z (a :- b :- c :- ds) = c
w (a :- b :- c :- d :- es) = d





xy :: (Vec a (S (S n))) -> Vec2 a
xy (a :- b :- bs) = a :- b :- Nil

swiz :: Vec Char n -> Vec a m -> Vec a n
swiz s (v :- vs) = case s of 'x' :- Nil -> (v :- Nil)

data MkNat :: Nat -> * where
    MkZ :: MkNat Z
    MkS :: MkNat n -> MkNat (S n)

takeFrom :: MkNat n -> [a] -> Vec a n
takeFrom MkZ _ = Nil
-- takeFrom _ [] = error "Trying to build vector from empty list"
takeFrom (MkS n) (a:as) = a :- takeFrom n as


data Some :: (k -> *) -> * where
    Like :: p x -> Some p

fromInt :: Int -> Some MkNat
fromInt 0 = Like MkZ
fromInt n = case fromInt (n-1) of Like n -> Like (MkS n)

class FromList n where
    fromList :: [a] -> Vec a n
    witness :: MkNat n

instance FromList Z where
    fromList [] = Nil
    witness = MkZ

instance FromList n => FromList (S n) where
    fromList (x:xs) = x :- fromList xs


