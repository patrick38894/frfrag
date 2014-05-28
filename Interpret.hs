{-# Language 
             FlexibleContexts,
             FlexibleInstances,
             GADTs,
             MultiParamTypeClasses,
             RankNTypes,
             TypeSynonymInstances
 #-}

module Interpret where

import Expressions
import Region
import Utility
import Statements
import Vector
import Control.Monad.State
import Data.Maybe
import Text.PrettyPrint.HughesPJ

type Interpret = State Fragment
data Fragment = Fragment {env :: Env, scope :: Scope, region :: Region, counter :: Int}
instance Pretty Fragment where
    pp frag =  pp (env frag)
           $+$ ppregion (region frag) (scope frag)
instance Pretty (Interpret a) where pp = pp . interpret 
instance Show Fragment where show = render . pp
instance Show (Interpret a) where show = render.pp
    
newtype Scope = Scope [Stmt]
instance Pretty Scope where
    pp (Scope xs) = vcat . map pp $ xs
data Env where
    Empty :: Env
    Extend :: (Pretty a, Wrap Expr a, Wrap Rep a) => Decl a -> Env -> Env    
instance Pretty Env where
    pp Empty = empty
    pp (Extend d ds) = pp d $+$ pp ds

ppregion :: Region -> Scope -> Doc
ppregion r xs = pp "void main ()"
    $+$ braceblock (case r of
        Anywhere -> pp xs)


makeSource :: Interpret a -> String
makeSource = show . interpret

lookUpProjection :: Eq b => (forall a . Decl a -> b) -> b -> Env -> Bool
lookUpProjection p s e = case e of
    Empty -> False
    Extend d' e' -> if s == p d' then True else lookUpProjection p s e'

lookUpDecl :: Decl a -> Env -> Maybe (Decl a)
lookUpDecl d e = case e of
    Empty -> Nothing
    Extend d' e' -> case d ~~ d' of 
                    Just Refl -> Just d'
                    Nothing -> lookUpDecl d e'

lookUpP :: Eq b => (forall a . Decl a -> b) -> b -> Interpret Bool
lookUpP p s = do
    frag <- get
    return $ lookUpProjection p s (env frag)

nameExists :: String -> Interpret Bool
nameExists = lookUpP declName

declName d = case d of
    Value (Var t n) e -> n
    Uniform (Var t n) e -> n

lookUpD :: Decl a -> Interpret (Maybe (Decl a))
lookUpD d = do
    frag <- get
    return $ lookUpDecl d (env frag)

extend :: (Pretty a, Wrap Expr a, Wrap Rep a) => Decl a -> Interpret (Decl a)
extend decl = do
    alreadyDefined <- lookUpD decl
    case alreadyDefined of
        Nothing -> do
            withState (\fr -> fr { env = Extend decl (env fr) }) (return decl)
        Just decl -> error $ "Redeclaration of " ++ show decl

getUniforms :: Env -> Env
getUniforms = getUniforms' Empty

getUniforms' :: Env -> Env -> Env
getUniforms' acc env = case env of
    Empty -> Empty
    Extend d env' -> case d of
        Uniform b e -> getUniforms' (Extend d acc) env'
        other -> getUniforms' acc env'

emptyFragment = Fragment {env = Empty, scope = Scope [], region = Anywhere, counter = 0}

interpret :: Interpret a -> Fragment
interpret e = execState e emptyFragment

declval :: (Wrap Rep a, Wrap Expr a, Pretty a) => (Binding a -> Maybe (Expr a) -> Decl a) -> 
           Binding a -> Maybe (Expr a) -> Interpret (Expr a)
declval f (v @ (Var tp nm)) def = do
    alreadyDefined <- nameExists nm
    if alreadyDefined
        then extend (f v def)
        else extend (f v def) -- error $ "Redeclaration of value " ++ nm
    return (Val v)
       
uniform :: (Wrap Expr a, Wrap Rep a, Pretty a) => Binding a -> Maybe (Expr a) -> Interpret (Expr a)
uniform = declval Uniform

newName :: Interpret String
newName = do
    frag <- get
    put $ frag {counter = counter frag + 1}
    return $ "generated" ++ show (counter frag)
 
value :: (Wrap Rep a, Wrap Expr a, Pretty a) => Expr a -> Interpret (Expr a)
value e = do
    name <- newName
    declval (\b d -> Value b (fromJust d)) (Var (wrap $ extract e) name) (Just e)

function :: Expr (a -> b) -> Interpret (Expr (a -> b))
function f = do
    case f of
        Prim s -> undefined
        Rewrite t u -> undefined
        App f e -> undefined
        Call r -> undefined
        other -> error "Invalid function expression"

setColor :: Expr (VecN Float) -> Interpret ()
setColor e = thenDo (Mutate FragColor e)

setRed :: Expr Float -> Interpret ()
setRed e = thenDo (Mutate (Swiz FragColor "x") e)

procedure :: Stmt -> Interpret (Expr a)
procedure = undefined

thenDo :: Stmt -> Interpret ()
thenDo s = do
    frag <- get
    put (case scope frag of Scope xs -> frag {scope = Scope $ xs ++ [s]})

firstDo :: Stmt -> Interpret ()
firstDo s = do
    frag <- get
    put (case scope frag of Scope xs -> frag {scope = Scope (s : xs)})
