module Main (main) where

import Evaluator
import Lexer (alexScanTokens)
import Parser (parseQuery)
import Control.Monad (forM_)
import Data.Char (isSpace)
import Data.List (sort, sortBy)
import Test.Hspec

main :: IO ()
main = hspec spec

data Fixture = Fixture
  { fixtureLabel :: String
  , fixtureQueryPath :: FilePath
  , fixtureExpectedPath :: FilePath
  }

spec :: Spec
spec = do
  describe "Coursework task queries" $ do
    forM_ courseworkTaskFixtures assertFixture

  describe "Language commands and operators" $ do
    forM_ languageFeatureFixtures assertFixture

  describe "Filter boundary behavior" $ do
    forM_ filterBoundaryFixtures assertFixture

assertFixture :: Fixture -> Spec
assertFixture f =
  it (fixtureLabel f) $ do
    actual <- runQueryFromFile (fixtureQueryPath f)
    expected <- loadExpectedLines (fixtureExpectedPath f)
    actual `shouldBe` expected

runQueryFromFile :: FilePath -> IO [String]
runQueryFromFile queryPath = do
  queryText <- readFile queryPath
  let query = parseQuery (alexScanTokens queryText)
  graph <- evalQuery query
  pure (map renderTriple (sortBy compareTriple (graphTriples graph)))

loadExpectedLines :: FilePath -> IO [String]
loadExpectedLines ttlPath = do
  contents <- readFile ttlPath
  pure (sort (filter (not . null) (map trim (lines contents))))

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

courseworkTaskFixtures :: [Fixture]
courseworkTaskFixtures =
  [ Fixture "runs Task 1 graph union" "test/task1_union.rql" "test/task1_union.ttl"
  , Fixture "runs Task 2 pattern match with >= filter" "test/task2_age_ge21.rql" "test/task2_age_ge21.ttl"
  , Fixture "runs Task 3 filter with == and ||" "test/task3_study_or_work.rql" "test/task3_study_or_work.ttl"
  , Fixture "runs Task 4 GROUP BY with MAX" "test/task4_max_price.rql" "test/task4_max_price.ttl"
  , Fixture "runs Task 5 graph edit join" "test/task5_graph_edit.rql" "test/task5_graph_edit.ttl"
  ]

languageFeatureFixtures :: [Fixture]
languageFeatureFixtures =
  [ Fixture "supports UNION keyword in WHERE patterns" "test/union_keyword.rql" "test/union_keyword.ttl"
  , Fixture "supports FILTER operators != and <" "test/filter_ne_lt.rql" "test/filter_ne_lt.ttl"
  , Fixture "supports FILTER operators >, <= and !" "test/filter_gt_le_not.rql" "test/filter_gt_le_not.ttl"
  , Fixture "supports MIN, COUNT and SUM aggregates with GROUP BY" "test/aggregates_min_count_sum.rql" "test/aggregates_min_count_sum.ttl"
  , Fixture "allows queries without WHERE" "test/no_where.rql" "test/no_where.ttl"
  , Fixture "supports list of triple patterns in WHERE (3-way join)" "test/where_three_way_join.rql" "test/where_three_way_join.ttl"
  , Fixture "supports list of triple patterns in OUTPUT" "test/output_triple_list.rql" "test/output_triple_list.ttl"
  ]

filterBoundaryFixtures :: [Fixture]
filterBoundaryFixtures =
  [ Fixture "includes the boundary value for >=" "test/boundary_ge.rql" "test/boundary_ge.ttl"
  , Fixture "excludes the boundary value for >" "test/boundary_gt.rql" "test/boundary_gt.ttl"
  , Fixture "includes the boundary value for <=" "test/boundary_le.rql" "test/boundary_le.ttl"
  , Fixture "supports NOT by inverting a boundary predicate" "test/not_inverts_boundary.rql" "test/not_inverts_boundary.ttl"
  , Fixture "supports nested NOT expressions" "test/nested_not.rql" "test/nested_not.ttl"
  ]
