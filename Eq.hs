{-# Language GADTs, PolyKinds #-}
module Eq where
import Language
import Control.Monad
------------------------------------------------------------------------------
data Refl :: k -> k -> k -> k -> * where
    Refl :: Refl a a b b

class RefEq f where
    (~~) :: f a b -> f a' b' -> Maybe (Refl a a b b)
    (~=) :: f a b -> f a' b' -> Bool
    a ~= b = case a ~~ b of Just Refl -> True; Nothing -> False
------------------------------------------------------------------------------

instance RefEq Rep where
    VoidT ~~ VoidT = Just Refl
    BoolT ~~ BoolT = Just Refl
    IntT ~~ IntT = Just Refl
    FloatT ~~ FloatT = Just Refl
    VecT r n ~~ VecT r' n' = do Refl <- r ~~ r'
                                guard (n == n')
                                Just Refl
    FuncT a r ~~ FuncT a' r' = do Refl <- a ~~ a'
                                  Refl <- r ~~ r'
                                  Just Refl
    a ~~ b = Nothing    

instance RefEq Binding where
    Void ~~ Void = Just Refl
    FragCoord ~~ FragCoord = Just Refl
    FragColor ~~ FragColor = Just Refl
    Var r s ~~ Var r' s'   = do Refl <- r ~~ r'
                                guard (s == s') 
                                Just Refl
    Func a r ~~ Func a' r' = do Refl <- a ~~ a'
                                Refl <- r ~~ r'
                                Just Refl
    a ~~ b = Nothing

instance RefEq Expr where
    Float x ~~ Float x' = do guard (x == x'); Just Refl
    Int x ~~ Int x' = do guard (x == x'); Just Refl
    Bool x ~~ Bool x' = do guard (x == x'); Just Refl
    Vec (VecT r n) x ~~ Vec (VecT r' n') x' = error "Vector RefEq not implemented"
    Val r ~~ Val r' = r ~~ r'
    Prim s ~~ Prim s' = do guard (s == s'); Just Refl
    Prim2 s ~~ Prim2 s' = do guard (s == s'); Just Refl
    BinOp s ~~ BinOp s' = do guard (s == s'); Just Refl
    App r b ~~ App r' b' = do Refl <- r ~~ r'
                              Refl <- b ~~ b'
                              Just Refl
    a ~~ b = Nothing

instance RefEq Decl where
    Value b e ~~ Value b' e' = do Refl <- b ~~ b'; Refl <- e ~~ e; Just Refl
    Uniform b (Just e) ~~ Uniform b' (Just e') = do Refl <- b ~~ b'; Refl <- e ~~ e; Just Refl
    Uniform b Nothing ~~ Uniform b' Nothing = b ~~ b'
------------------------------------------------------------------------------
instance Eq (Rep a b) where a == b = a ~= b
instance Eq (Binding a b) where a == b = a ~= b
instance Eq (Expr a b) where a == b = a ~= b
------------------------------------------------------------------------------
