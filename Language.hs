{-# Language
        DataKinds,
        FlexibleInstances,
        GADTs,
        KindSignatures,
        MultiParamTypeClasses,
        PolyKinds,
        StandaloneDeriving,
        TypeSynonymInstances #-}

module Language where
------------------------------------------------------------------------------
data V2 a = Vec2 a a
data V3 a = Vec3 a a a
data V4 a = Vec4 a a a a

class Vec v where dim :: v a -> Int
class Vec2p v
class Vec3p v
class Vec4p v

instance Vec V2 where dim = const 2
instance Vec V3 where dim = const 3
instance Vec V4 where dim = const 4

instance Vec2p V2
instance Vec2p V3
instance Vec2p V4
instance Vec3p V3
instance Vec3p V4
instance Vec4p V4

class Expr repr where
    bool    :: Bool -> repr Bool
    int     :: Int -> repr Int
    float   :: Float -> repr Float
    vec     :: Vec v => v (repr a) -> repr (v a)

class BinOp repr where
    mulOp   :: String -> repr (a -> a -> a)
    addOp   :: String -> repr (a -> a -> a)
    compOp  :: String -> repr (a -> a -> Bool)

class Prim repr where
    prim    :: String -> repr (a -> a)
    prim2   :: String -> repr (a -> a -> a)
    
class Rewrite repr where
    lam     :: (repr a -> repr b) -> repr (a -> b)
    app     :: repr (a -> b) -> repr a -> repr b

class Decl repr where
    uni     :: repr a
    val     :: repr a -> repr a
    call    :: repr (a -> b) -> repr (a -> b)
    proc    :: stmt (a -> b) -> repr (a -> b)

class Stmt stmt where
    local   :: decl a -> stmt ()
    seq     :: stmt a -> stmt a -> stmt a
    ifElse  :: repr Bool -> stmt a -> stmt a -> stmt a
    caseOf  :: [(Int, stmt a)] -> stmt a
    for     :: repr Int -> repr (Int -> Bool) -> repr (Int -> Int) -> stmt a -> stmt a
    while   :: repr Bool -> stmt a -> stmt a
    break   :: stmt ()
    cont    :: stmt ()
    ret     :: repr a -> stmt a
    halt    :: stmt ()
    discard :: stmt ()
    noOp    :: stmt ()
