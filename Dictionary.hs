module Dictionary where
import Grammar

type Dictionary k v = [(k, v)]

getValue :: Eq k => Dictionary k v -> k -> Maybe v
getValue [] _ = Nothing
getValue ((a, b):xs) k
    | a == k    = Just b
    | otherwise = getValue xs k

setValue :: Eq k => Dictionary k v -> (k, v) -> Dictionary k v
setValue [] e = [e]
setValue (x@(a, b):xs) (k, v)
    | a == k = (k, v):xs
    | otherwise = x : setValue xs (k, v)

type State = (Dictionary Char Integer, Dictionary Integer Stmt, [Integer], Integer)

getVars :: State -> Dictionary Char Integer
getVars (a, b, c, d) = a

setVar :: State -> (Char, Integer) -> State
setVar (va, p, st, pc) (k, v) = (setValue va (k, v), p, st, pc)

getVar :: State -> Char -> Maybe Integer
getVar = getValue . getVars

getProgram :: State -> Dictionary Integer Stmt
getProgram (a, b, c, d) = b

getStack :: State -> [Integer]
getStack (a, b, c, d) = c

pushStack :: State -> Integer -> State
pushStack (va, p, st, pc) x = (va, p, x:st, pc)

getPC :: State -> Integer
getPC (a, b, c, d) = d

setPC :: State -> Integer -> State
setPC (va, p, st, pc) x = (va, p, st, x)