module Grammar where
import Parser

data Expr = Val Integer | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Var Char deriving Show

-- expression ::= (+|-|ε) term ((+|-) term)*
-- term ::= factor ((*|/) factor)*
-- factor ::= var | number | (expression)
-- number ::= 0 | 1 | 2 | 3 | ...
-- var ::= A | B | C | ... | Z

expression :: Parser Expr
expression = do
    t <- term
    symbol "+"
    Add t <$> term
    <|> do
    t <- term
    symbol "-"
    Sub t <$> term
    <|>
    term

term :: Parser Expr
term = do
    f <- factor
    symbol "*"
    Mul f <$> factor
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