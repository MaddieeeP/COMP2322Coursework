import Lexer
import Parser
import Evaluator_v2
import System.IO 
import System.Environment (getArgs)

main :: IO ()
main = do
    (filename:_) <- getArgs
    text <- readFile filename
    -- print text
    let tokens = alexScanTokens text 
    let ast = parseQuery tokens
    mapM_ (putStrLn . printToken) tokens
    print ast
    solution <- evalQuery ast
    print solution

printToken :: TokenPosn -> String
printToken (PT (AlexPn _ line col) tok) =
    show tok ++ " at " ++ show line ++ ":" ++ show col