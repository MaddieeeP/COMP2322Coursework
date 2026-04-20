import Lexer
import Parser
import Evaluator
import AST (Triple(Triple), Term(..))
import Data.List (sortOn)
import System.Environment (getArgs)

main :: IO ()
main = do
    (filename:_) <- getArgs
    text <- readFile filename
    let tokens = alexScanTokens text 
    let ast = parseQuery tokens
    solution <- evalQuery ast
    mapM_ (putStrLn . renderTriple) (sortOn tripleSortKey (graphTriples solution))

tripleSortKey :: Triple -> (String, String, String)
tripleSortKey (Triple subj predicateTerm obj) =
    (renderTerm subj, renderTerm predicateTerm, renderTerm obj)

renderTriple :: Triple -> String
renderTriple (Triple subj predicateTerm obj) =
    renderTerm subj ++ " " ++ renderTerm predicateTerm ++ " " ++ renderTerm obj ++ " ."

renderTerm :: Term -> String
renderTerm (URI u) = "<" ++ u ++ ">"
renderTerm (LitString strVal) = show strVal
renderTerm (LitInt i) = show i