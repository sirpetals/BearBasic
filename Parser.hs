{- HLINT ignore "Use const" -}
{- HLINT ignore "Use lambda-case" -}
module Parser where

newtype Parser a = P (String -> Maybe (a, String))

class Applicative f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a

parse :: Parser a -> String -> Maybe (a, String)
parse (P pa) = pa

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap fab pa = do
        a <- pa
        return (fab a)

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure a = P (\cs -> Just (a, cs))

    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (<*>) pfab pa = do
        fab <- pfab
        fab <$> pa

instance Monad Parser where
    -- return :: a -> Parser a
    return = pure

    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (>>=) pa fapb = P (\cs ->
        case parse pa cs of
            Nothing -> Nothing
            Just (a, cs') -> parse (fapb a) cs')

instance Alternative Parser where
    empty = P (\cs -> Nothing)

    (<|>) :: Parser a -> Parser a -> Parser a
    pa <|> pb = P (\cs ->
        case parse pa cs of
            Nothing -> parse pb cs
            Just (a, cs') -> Just (a, cs'))

one :: Parser Char
one = P (\cs ->
    case cs of
        []      -> Nothing
        (c:cs') -> Just (c,cs'))

oneIf :: (Char -> Bool) -> Parser Char
oneIf p = do
    x <- one
    if p x then
        return x
    else
        P (\cs -> Nothing)

-- 1 or more
some :: Parser a -> Parser [a]
some pa = (:) <$> pa <*> many pa

-- 0 or more
many :: Parser a -> Parser [a]
many pa = some pa <|> pure []

digit :: Parser Char
digit = oneIf (`elem` ['0'..'9'])

nat :: Parser Integer
nat = read <$> some digit

-- +val | -val | val
int :: Parser Integer
int = do
    symbol "-"
    n <- nat
    return (- (1 * n))
    <|> do
    symbol "+"
    nat
    <|>
    nat

-- Parse a specific character
char :: Char -> Parser Char
char c = oneIf (== c)

string :: String -> Parser String
string [] = pure []
string (x:xs) = do
    c <- char x
    cs <- string xs
    return (c:cs)

space :: Parser ()
space = do
    many $ char ' '
    return ()

token :: Parser a -> Parser a
token pa = do
    space
    a <- pa
    space
    return a

symbol :: String -> Parser String
symbol xs = token $ string xs