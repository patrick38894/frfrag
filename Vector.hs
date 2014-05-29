module Vector where

data N = N2 
       | N3 
       | N4
    deriving Eq

data VecN a = Vec2 a a
            | Vec3 a a a
            | Vec4 a a a a
    deriving Eq

intN :: N -> Int
intN n = case n of N2 -> 2; N3 -> 3; N4 -> 4

vecToList :: VecN a -> [a]
vecToList v = case v of
    Vec2 a b     -> [a,b]
    Vec3 a b c   -> [a,b,c]
    Vec4 a b c d -> [a,b,c,d]

vecFromList :: [a] -> VecN a
vecFromList xs = case xs of
   [a,b]     ->  Vec2 a b        
   [a,b,c]   ->  Vec3 a b c      
   [a,b,c,d] ->  Vec4 a b c d    


