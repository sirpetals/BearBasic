module Grammar where
import Parser (Alternative(..), Parser, oneIf, token, symbol, int)

data Stmt = Print [Expr] | Input [Expr] | If Relop Expr Expr Stmt | Goto Expr 
    | Gosub Expr | Return | Let Expr Expr | List | Run | End deriving Show

statement :: Parser Stmt
statement = do
    symbol "PRINT"
    Print <$> expressionList
    <|> do
    symbol "IF"
    le <- expression
    r <- relop
    re <- expression
    symbol "THEN"
    If r le re <$> statement
    <|> do
    symbol "GOTO"
    Goto <$> expression
    <|> do
    symbol "GOSUB"
    Gosub <$> expression
    <|> do
    symbol "RETURN"
    return Return
    <|> do
    symbol "INPUT"
    Input <$> varList
    <|> do
    symbol "LET"
    v <- variable
    symbol "="
    Let v <$> expression
    <|> do
    symbol "LIST"
    return List
    <|> do
    symbol "RUN"
    return Run
    <|> do
    symbol "END"
    return End

data Expr = Val Integer | Add Expr Expr | Sub Expr Expr | Mul Expr Expr
    | Div Expr Expr | Var Char deriving Show

data Relop = Gt | Gte | Lt | Lte | Eq | Ne deriving Show

relop :: Parser Relop
relop = do
    symbol ">="
    return Gte
    <|> do
    symbol "<="
    return Lte
    <|> do
    symbol "<>"
    return Ne
    <|> do
    symbol ">"
    return Gt
    <|> do
    symbol "<"
    return Lt
    <|> do
    symbol "="
    return Eq

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

varList :: Parser [Expr]
varList = do
    v <- variable
    symbol ","
    (v:) <$> varList
    <|> do
    (:[]) <$> variable