module Evaluator
  ( Graph(..)
  , Binding
  , Solutions
  , emptyGraph
  , unionGraph
  , evalQuery
  , renderTriple
  , renderTerm
  , compareTriple
  , compareTerm
  ) where

import qualified Data.Text as T
import qualified Data.RDF as RDF
import Data.RDF.Graph.TList
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad (foldM)
import Data.Maybe (mapMaybe)
import AST

newtype Graph = Graph
  { graphTriples :: [Triple]
  } deriving (Eq, Show)

-- Variable bindings from pattern matches
type Binding = Map.Map VarName Term
type Solutions = [Binding]

-- Groups used for aggregates
type Group = [Binding]
type GroupedSolutions = [Group]

emptyGraph :: Graph
emptyGraph = Graph []

unionGraph :: Graph -> Graph -> Graph
unionGraph (Graph left) (Graph right) = Graph (dedup (left ++ right))

dedup :: Ord a => [a] -> [a]
dedup = Set.toList . Set.fromList

evalQuery :: Query -> IO Graph
evalQuery query = do
  let sources = querySources query
  sourceGraphs <- mapM loadSource sources
  let sourceNames = [name | File name <- sources]
  let namedGraphs = Map.fromList (zip sourceNames sourceGraphs)
  let inputGraph = foldr unionGraph emptyGraph sourceGraphs
  let matchedSolutions = case queryWhere query of
        Nothing -> [Map.empty]
        Just whereClause -> matchPattern (wherePattern whereClause) inputGraph namedGraphs
  let groupedSolutions =
        case queryWhere query of
          Nothing -> [matchedSolutions]
          Just whereClause -> groupForAgg (whereGroupBy whereClause) matchedSolutions
  let outputTriples = makeOutput (queryOutput query) groupedSolutions
  pure (Graph (dedup outputTriples))

-- Load a source graph from <name>.ttl in the current working directory.
loadSource :: Source -> IO Graph
loadSource (File fname) = do
  contents <- readFile (fname ++ ".ttl")
  pure (Graph (parseTurtle contents))

-- Parse Turtle with rdf4h and convert into internal triples.
parseTurtle :: String -> [Triple]
parseTurtle input =
  either (error . ("Turtle parse error: " ++) . show) (map (\t -> Triple (toTerm $ RDF.subjectOf t) (toTerm $ RDF.predicateOf t) (toTerm $ RDF.objectOf t)) . RDF.triplesOf) parsed
  where
    parsed = RDF.parseString (RDF.TurtleParser Nothing Nothing) (T.pack input) :: Either RDF.ParseFailure (RDF.RDF TList)
    toTerm (RDF.UNode uri) = URI (T.unpack uri)
    toTerm (RDF.BNode blank) = URI ("_:" ++ T.unpack blank)
    toTerm (RDF.LNode lit) = case lit of
      RDF.TypedL txt dType | T.unpack dType == "http://www.w3.org/2001/XMLSchema#integer" -> LitInt (read $ T.unpack txt)
      RDF.TypedL txt _ -> LitString (T.unpack txt)
      RDF.PlainL txt -> LitString (T.unpack txt)
      RDF.PlainLL txt _ -> LitString (T.unpack txt)
    toTerm n = error ("Unexpected RDF node: " ++ show n)




-- Handle basic union and filtered patterns
matchPattern :: Pattern -> Graph -> Map.Map String Graph -> Solutions
matchPattern (Basic triplePatterns) graph _ = matchBasic triplePatterns graph
matchPattern (Union left right) graph ng =
  matchPattern left graph ng ++ matchPattern right graph ng
matchPattern (Join left right) graph ng =
  let lSols = matchPattern left graph ng
      rSols = matchPattern right graph ng
  in [merged | ls <- lSols, rs <- rSols, Just merged <- [merge ls rs]]
matchPattern (Filtered pattern expr) graph ng =
  applyFilterExpr expr (matchPattern pattern graph ng)
matchPattern (Scoped name pattern) _ ng =
  case Map.lookup name ng of
    Just g  -> matchPattern pattern g ng
    Nothing -> error ("GRAPH reference to unknown source: " ++ name)

-- Match one term and return a binding piece
matchTerm :: TermOrVar -> Term -> Maybe Binding
matchTerm (Var x) t = Just (Map.singleton x t)
matchTerm (Concrete y) t =
  if y == t
    then Just Map.empty
  else Nothing
matchTerm (Agg _) _ = error "Aggregate functions not allowed in WHERE pattern"

-- Merge one assignment into a binding
mergeStep :: Binding -> (VarName, Term) -> Maybe Binding
mergeStep acc (x, t) =
  case Map.lookup x acc of
    Nothing -> Just (Map.insert x t acc)
    Just t' ->
      if t' == t
        then Just acc
        else Nothing

-- Merge two bindings and fail on conflicts
merge :: Binding -> Binding -> Maybe Binding
merge b1 b2 = foldM mergeStep b1 (Map.toList b2)

-- Match one triple pattern against one triple
matchTriple :: TriplePattern -> Triple -> Maybe Binding
matchTriple (TriplePattern ps pp pq) (Triple s p q) = do
  b1 <- matchTerm ps s
  b2 <- matchTerm pp p
  b3 <- matchTerm pq q
  pair <- merge b1 b2
  merge pair b3

-- Apply a binding to one term
bindTerm :: Binding -> TermOrVar -> TermOrVar
bindTerm binding (Var v) =
  case Map.lookup v binding of
    Just t  -> Concrete t
    Nothing -> Var v
bindTerm _ tov = tov

-- Apply a binding to all terms in a triple pattern
bindPattern :: Binding -> TriplePattern -> TriplePattern
bindPattern binding (TriplePattern s p o) =
  TriplePattern (bindTerm binding s)
                (bindTerm binding p)
                (bindTerm binding o)

-- Match one pattern against all triples
matchOnePattern :: TriplePattern -> [Triple] -> Solutions
matchOnePattern pat triples =
    mapMaybe (matchTriple pat) triples

-- Join basic patterns left to right
matchBasic :: [TriplePattern] -> Graph -> Solutions
matchBasic pats (Graph triples) =
  foldl step [Map.empty] pats
  where
    step sols p =
      concatMap (\sol ->
        map (Map.union sol) (matchOnePattern (bindPattern sol p) triples)
      ) sols



compareTerms :: Operator -> Term -> Term -> Bool
compareTerms Eq a b = a == b
compareTerms Ne a b = a /= b
compareTerms Ge (LitInt a) (LitInt b) = a >= b
compareTerms Gt (LitInt a) (LitInt b) = a > b
compareTerms Le (LitInt a) (LitInt b) = a <= b
compareTerms Lt (LitInt a) (LitInt b) = a < b
compareTerms Ge (LitString a) (LitString b) = a >= b
compareTerms Gt (LitString a) (LitString b) = a > b
compareTerms Le (LitString a) (LitString b) = a <= b
compareTerms Lt (LitString a) (LitString b) = a < b
compareTerms _ _ _ = False

-- Resolve a term with one binding
resolveTerm :: TermOrVar -> Binding -> Maybe Term
resolveTerm (Concrete t) _ = Just t
resolveTerm (Var v) binding = Map.lookup v binding
resolveTerm (Agg _) _ = Nothing

-- Evaluate a filter on one binding
evalFilter :: FilterExpr -> Binding -> Bool
evalFilter (Comp op left right) bd = case (resolveTerm left bd, resolveTerm right bd) of
    (Just l, Just r) -> compareTerms op l r
    _                -> False
evalFilter (And a b) bd = evalFilter a bd && evalFilter b bd
evalFilter (Or a b) bd = evalFilter a bd || evalFilter b bd
evalFilter (Not a) bd = not (evalFilter a bd)


-- Keep only bindings that pass the filter
applyFilterExpr :: FilterExpr -> Solutions -> Solutions
applyFilterExpr f = filter (evalFilter f)


partition :: VarName -> Solutions -> [[Binding]]
partition v sols =
  let
    pairs = [(Map.lookup v b, [b]) | b <- sols]
    grouped = Map.fromListWith (++) pairs
  in
    Map.elems grouped


-- Group bindings for aggregate output
groupForAgg :: Maybe VarName -> Solutions -> GroupedSolutions
groupForAgg maybeVar sols =
  case maybeVar of
    Just v  -> partition v sols
    Nothing -> [sols]


-- Get values for one variable in a group
vals :: VarName -> Group -> [Term]
vals v g = mapMaybe (Map.lookup v) g

-- Helper for aggregates over any term
withVals :: VarName -> Group -> ([Term] -> Maybe Term) -> Maybe Term
withVals v g f =
  case vals v g of
    [] -> Nothing
    xs -> f xs

-- Helper for integer aggregates
withInts :: VarName -> Group -> ([Integer] -> Maybe Term) -> Maybe Term
withInts v g f =
  let ints = [n | LitInt n <- vals v g]
  in
    case ints of
      [] -> Nothing
      nums -> f nums

-- Evaluate one aggregate over a group
evalAgg :: AggFunc -> Group -> Maybe Term
evalAgg agg g =
  case agg of
    Max v -> withInts v g (\nums -> Just (LitInt (maximum nums)))
    Min v -> withInts v g (\nums -> Just (LitInt (minimum nums)))
    Count v -> withVals v g (\xs -> Just (LitInt (fromIntegral (length xs))))
    Sum v -> withInts v g (\nums -> Just (LitInt (sum nums)))


-- Resolve output terms using binding and group
resolveOutput :: Group -> Binding -> TermOrVar -> Maybe Term
resolveOutput _ _ (Concrete t) = Just t
resolveOutput _ b (Var v) = Map.lookup v b
resolveOutput grp _ (Agg agg) = evalAgg agg grp



-- Build one output triple
makeTriple :: Group -> TriplePattern -> Binding -> Maybe Triple
makeTriple grp (TriplePattern ps pp po) b =
  case (resolveOutput grp b ps, resolveOutput grp b pp, resolveOutput grp b po) of
    (Just s, Just p, Just o) -> Just (Triple s p o)
    _ -> Nothing

-- Build output triples for one binding
makeTriples :: Group -> [TriplePattern] -> Binding -> [Triple]
makeTriples grp out b = [t | tp <- out, Just t <- [makeTriple grp tp b]]

termHasAgg :: TermOrVar -> Bool
termHasAgg (Agg _) = True
termHasAgg _ = False

outputHasAgg :: Output -> Bool
outputHasAgg (Output patterns) = any hasAggPattern patterns
  where
    hasAggPattern (TriplePattern subj pred obj) =
      termHasAgg subj || termHasAgg pred || termHasAgg obj

groupOutputBindings :: Output -> Group -> [Binding]
groupOutputBindings out grp
  | outputHasAgg out = dedup grp
  | otherwise = grp

-- Build final output triples
makeOutput :: Output -> GroupedSolutions -> [Triple]
makeOutput out@(Output patterns) grouped =
  concatMap (\grp -> concatMap (makeTriples grp patterns) (groupOutputBindings out grp)) grouped

-- Render a term in canonical N-Triples form.
renderTerm :: Term -> String
renderTerm (URI u) = "<" ++ u ++ ">"
renderTerm (LitString strVal) = show strVal
renderTerm (LitInt i) = show i

-- Render a triple in canonical N-Triples form.
renderTriple :: Triple -> String
renderTriple (Triple subj predicateTerm obj) =
  renderTerm subj ++ " " ++ renderTerm predicateTerm ++ " " ++ renderTerm obj ++ " ."

-- Ordering per spec: strings < ints < URIs, default Haskell Ord within each type.
compareTerm :: Term -> Term -> Ordering
compareTerm (LitString a) (LitString b) = compare a b
compareTerm (LitInt a) (LitInt b) = compare a b
compareTerm (URI a) (URI b) = compare a b
compareTerm (LitString _) _ = LT
compareTerm _ (LitString _) = GT
compareTerm (LitInt _) _ = LT
compareTerm _ (LitInt _) = GT

compareTriple :: Triple -> Triple -> Ordering
compareTriple (Triple s1 p1 o1) (Triple s2 p2 o2) =
  compareTerm s1 s2 <> compareTerm p1 p2 <> compareTerm o1 o2