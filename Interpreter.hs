{- HLINT ignore "Use tuple-section" -}
{- HLINT ignore "Used otherwise as a pattern" -}
module Interpreter where

import Grammar (Expr (..), Stmt (..), Relop (..))
import Dictionary (Dictionary, getValue, setValue, State, setVar, getVar, pushStack, getPC, setPC)

newtype ST a = S (State -> Maybe (a, State))

apply :: ST a -> State -> Maybe (a, State)
apply (S st) = st

data Result = Num Integer | Str String | Output String | Empty | Quit deriving Show

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

instance Monad ST where
    return :: a -> ST a
    return = pure
    
    (>>=) :: ST a -> (a -> ST b) -> ST b
    sta >>= fastb = S (\s ->
        case apply sta s of
            Nothing -> Nothing
            Just (a, s') -> apply (fastb a) s')

interpretExpr :: Expr -> ST Integer
interpretExpr (Val x)   = S (\s -> Just (x, s))
interpretExpr (Var v)   = S (\s ->
    case getVar s v of
        Nothing -> Nothing
        Just a -> Just (a, s))
interpretExpr (Add a b) = (+) <$> interpretExpr a <*> interpretExpr b
interpretExpr (Sub a b) = (-) <$> interpretExpr a <*> interpretExpr b
interpretExpr (Mul a b) = (*) <$> interpretExpr a <*> interpretExpr b
interpretExpr (Div a b) = div <$> interpretExpr a <*> interpretExpr b

interpretStmt :: Stmt -> ST Result
interpretStmt (Print es) = S (\s ->
    case listExprs es s of
        Left err -> Just (Output err, s)
        Right smth -> Just (Output (foldr (\a b -> a ++ " " ++ b) "" smth), s))

interpretStmt (If op le re st) = do
    lr <- interpretExpr le
    rr <- interpretExpr re
    if f lr rr then interpretStmt st else return Empty
    where
        f = relopToF op

interpretStmt (Goto e) = S (\s ->
    case apply (interpretExpr e) s of
        Nothing -> Nothing
        Just (v, s') -> Just (Empty, setPC (pushStack s' (getPC s')) v))

interpretStmt (Let (Var a) b) = S (\s ->
    case apply (interpretExpr b) s of
        Nothing -> Nothing
        Just (x, s') -> Just (Empty, setVar s' (a, x)))

interpretStmt End = S (\s -> Just (Quit, s))

interpretStmt _ = S (\s -> Just (Output "Statement not yet implemented.", s))

listExprs :: [Expr] -> State -> Either String [[Char]]
listExprs [] _ = Right []
listExprs (e:es) s =
    case apply (interpretExpr e) s of
        Nothing -> Left "Invalid element in expression list."
        Just (v, s') -> (show v :) <$> listExprs es s'


relopToF :: Relop -> (Integer -> Integer -> Bool)
relopToF Gt  = (>)
relopToF Gte = (>=)
relopToF Lt  = (<)
relopToF Lte = (<=)
relopToF Eq  = (==)
relopToF Ne  = (/=)