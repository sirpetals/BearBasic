{- HLINT ignore "Use tuple-section" -}
module Interpreter where

import Grammar (Expr (..), Stmt (..))
import Dictionary (Dictionary, getValue, setValue)

type State = Dictionary Char Integer
newtype ST a = S (State -> Maybe (a, State))

class Interpretable f where
    interpret :: f -> ST Integer

instance Functor ST where
    fmap :: (a -> b) -> ST a -> ST b
    fmap fab sta = S (\s ->
        case apply sta s of
            Nothing -> Nothing
            Just (a, s') -> Just (fab a, s'))

instance Applicative ST where
    pure :: a -> ST a
    pure a = S (\s -> Just (a, s))

    (<*>) :: ST (a -> b) -> ST a -> ST b
    stfab <*> sta = S (\s ->
        case apply stfab s of
            Nothing -> Nothing
            Just (fab, s') -> apply (fab <$> sta) s')

instance Interpretable Expr where
    interpret :: Expr -> ST Integer
    interpret (Val x)   = S (\s -> Just (x, s))
    interpret (Var v)   = S (\s ->
        case getValue s v of
            Nothing -> Nothing
            Just a -> Just (a, s))
    interpret (Add a b) = (+) <$> interpret a <*> interpret b
    interpret (Sub a b) = (-) <$> interpret a <*> interpret b
    interpret (Mul a b) = (*) <$> interpret a <*> interpret b
    interpret (Div a b) = div <$> interpret a <*> interpret b

instance Interpretable Stmt where
    interpret :: Stmt -> ST Integer
    interpret (Let (Var a) b) = S (\s -> 
        case apply (interpret b) s of
            Nothing -> Nothing
            Just (x, s') -> Just (x, setValue s' (a, x)))

apply :: ST a -> State -> Maybe (a, State)
apply (S st) = st