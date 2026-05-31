module BearBasic where
import Interpreter (apply, Interpretable (..), State)
import Parser (parse)
import Grammar (statement)

loop :: State -> IO ()
loop s = do
    str <- getLine
    case parse statement str of
        Nothing -> do
            putStrLn "Parse error."
            loop s
        Just (stmt, _) -> do
            case apply (interpret stmt) s of
                Nothing -> do 
                    putStrLn "Execution error."
                    loop s
                Just (result, s') -> loop s'

    return ()