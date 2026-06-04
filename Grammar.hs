module Grammar where
import Parser (Alternative(..), Parser, oneIf, token, symbol, int)

-- statement ::= PRINT expr-list
--               IF expression relop expression THEN statement
--               GOTO expression
--               INPUT var-list
--               LET var = expression
--               GOSUB expression
--               RETURN
--               CLEAR
--               LIST
--               RUN
--               END

data Relop = GT | GTE | LT | LTE | NE | EQ deriving Show
data Stmt = Print [Expr] | If Expr Relop Expr Stmt | Let Expr Expr | End deriving Show

statement :: Parser Stmt
statement = do
    symbol "PRINT"
    Print <$> expressionList
    <|> do
    symbol "LET"
    v <- variable
    symbol "="
    Let v <$> expression
    <|> do
    symbol "END"
    return End

data Expr = Val Integer | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr | Var Char deriving Show

-- expression ::= term + expression | term - expression | term
-- expr-list  ::= (string|expression) (, (string|expression) )*
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
    token term

expressionList :: Parser [Expr]
expressionList = do
    e <- expression
    symbol ","
    (e:) <$> expressionList
    <|> do
    (:[]) <$> expression

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
    token factor

factor :: Parser Expr
factor = do
    symbol "("
    e <- expression
    symbol ")"
    return e
    <|>
    token number
    <|>
    token variable

number :: Parser Expr
number = do
    Val <$> int

variable :: Parser Expr
variable = do
    Var <$> token (oneIf (`elem` ['A' .. 'Z']))