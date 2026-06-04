module BearBasic where
import Interpreter (apply, Interpretable (..), State)
import Parser (parse)
import Grammar (statement, Stmt (..), Expr, expression)

loop :: State -> IO ()
loop s = do
    str <- getLine
    case parse statement str of
        Nothing -> do
            putStrLn "Parse error."
            loop s
        Just (stmt, _) -> do
            case stmt of
                Print es -> do
                    printList es s
                    loop s
                End ->
                    return ()
                _ ->
                    case apply (interpret stmt) s of
                        Nothing -> do 
                            putStrLn "Execution error."
                            loop s
                        Just (result, s') -> loop s'

    return ()

printList :: [Expr] -> State -> IO ()
printList [] s = putChar '\n'
printList (e:es) s = 
    case apply (interpret e) s of
        Nothing -> putStrLn "Invalid element in expression list."
        Just (v, s') ->
            do
                putStr $ show v
                printList es s
    
