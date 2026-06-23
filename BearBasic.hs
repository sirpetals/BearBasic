module Main where
import Interpreter (apply, interpretExpr, interpretStmt, State, Result (..))
import Parser (parse)
import Grammar (statement, Stmt (..), Expr, expression)

main :: IO ()
main = loop []

loop :: State -> IO ()
loop s = do
    str <- getLine
    case parse statement str of
        Nothing -> do
            putStrLn "Parse error."
            loop s
        Just (stmt, "") ->
            case apply (interpretStmt stmt) s of
                Nothing -> do 
                    putStrLn "Execution error."
                    loop s
                Just (result, s') ->
                    case result of
                        Output str -> do
                            putStrLn str
                            loop s'
                        Empty -> loop s'
                        Quit -> return ()
        _ -> do
            putStrLn "Parse error."
            loop s

printList :: [Expr] -> State -> IO ()
printList [] s = putChar '\n'
printList (e:es) s = 
    case apply (interpretExpr e) s of
        Nothing -> putStrLn "Invalid element in expression list."
        Just (v, s') ->
            do
                putStr $ show v ++ " "
                printList es s
    
