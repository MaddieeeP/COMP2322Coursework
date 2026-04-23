import Lexer
import Parser
import Evaluator
import Data.List (sortBy)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        (filename:_) -> do
            text <- readFile filename
            let tokens = alexScanTokens text
            let ast = parseQuery tokens
            solution <- evalQuery ast
            mapM_ (putStrLn . renderTriple) (sortBy compareTriple (graphTriples solution))
        [] -> error "Usage: tsparq <query-file>"
