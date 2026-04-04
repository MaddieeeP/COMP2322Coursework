module AST
  ( VarName
  , Query(..)
  , Source(..)
  , Output(..)
  , Where(..)
  , Pattern(..)
  , TriplePattern(..)
  , Triple(..)
  , Term(..)
  , TermOrVar(..)
  , FilterExpr(..)
  , CompOp(..)
  , AggregateFunc(..)
  ) where

type VarName = String

-- Query: input graphs, output shape and optional where
data Query = Query
  { querySources   :: [Source]
  , queryOutput    :: Output
  , queryWhere     :: Maybe Where
  } deriving (Eq, Show)

data Source = File String
  deriving (Eq, Show)

-- output in triple shape
data Output = Output [TriplePattern]
  deriving (Eq, Show)

-- optional grouping.
data Where = Where
  { wherePattern :: Pattern
  , whereGroupBy :: Maybe VarName
  } deriving (Eq, Show)

-- Recursive pattern tree: basic block, union, or filtered pattern
data Pattern
  = Basic [TriplePattern]
  | Union Pattern Pattern
  | Filtered Pattern FilterExpr
  deriving (Eq, Show)

-- Triple shape that allows variables
data TriplePattern = TriplePattern
  { subject   :: TermOrVar
  , predicate :: TermOrVar
  , object    :: TermOrVar
  } deriving (Eq, Show)

-- RDF triple
data Triple = Triple
  { subject   :: Term
  , predicate :: Term
  , object    :: Term
  } deriving (Eq, Show)

-- RDF value.
data Term
  = URI String
  | LitString String
  | LitInt Integer
  deriving (Eq, Show)


data TermOrVar
  = Concrete Term
  | Var VarName
  | Agg AggFunc
  deriving (Eq, Show)

data FilterExpr
  = Comp Operator TermOrVar TermOrVar
  | And FilterExpr FilterExpr
  | Or FilterExpr FilterExpr
  | Not FilterExpr
  deriving (Eq, Show)

-- Comparison operators used in filters
data Operator
  = Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge
  deriving (Eq, Show)


data AggFunc
  = Max VarName
  | Min VarName
  | Count VarName
  | Sum VarName
  deriving (Eq, Show)