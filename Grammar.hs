module Grammar where
import Parser (Alternative(..), Parser, oneIf, token, symbol, int)

data Stmt = Let Char Integer deriving Show
data Expr = Val Integer | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr | Var Char deriving Show

-- expression ::= term + expression | term - expression | term
-- term       ::= factor * term | factor / term | factor
-- factor     ::= var | number | (expression)
-- number     ::= (+|-|ε) 0 | 1 | 2 | 3 | ...
-- var        ::= A | B | C | ... | Z

expression :: Parser Expr
expression = do
    t <- term
    symbol "+"
    Add t <$> expression
    <|> do
    t <- term
    symbol "-"
    Sub t <$> expression
    <|>
    term

term :: Parser Expr
term = do
    f <- factor
    symbol "*"
    Mul f <$> term
    <|> do
    f <- factor
    symbol "/"
    Div f <$> term
    <|>
    factor

factor :: Parser Expr
factor = do
    symbol "("
    e <- expression
    symbol ")"
    return e
    <|>
    number
    <|>
    variable

number :: Parser Expr
number = do
    Val <$> int

variable :: Parser Expr
variable = do
    Var <$> token (oneIf (`elem` ['A' .. 'Z']))