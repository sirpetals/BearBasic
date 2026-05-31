module Dictionary where

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