{ 
module Lexer where 
}

%wrapper "posn" 
$digit = 0-9     
$alpha = [a-zA-Z]    

tokens :-
$white+       ; 
SOURCE      { \p s -> PT p TSOURCE }
OUTPUT      { \p s -> PT p TOUTPUT }
WHERE       { \p s -> PT p TWHERE }
GROUP       { \p s -> PT p TGROUP }
BY          { \p s -> PT p TBY }
UNION       { \p s -> PT p TUNION }
FILTER      { \p s -> PT p TFILTER }

MAX         { \p s -> PT p TMAX }
MIN         { \p s -> PT p TMIN }
COUNT       { \p s -> PT p TCOUNT }
SUM         { \p s -> PT p TSUM }

\.         { \p s -> PT p TDot }
\{         { \p s -> PT p TLBrace }
\}         { \p s -> PT p TRBrace }
\(         { \p s -> PT p TLParen }
\)         { \p s -> PT p TRParen }

==        { \p s -> PT p TEq }
\>=        { \p s -> PT p TGe }
\<=        { \p s -> PT p TLe }
!=        { \p s -> PT p TNe }
\>         { \p s -> PT p TGt }
\<         { \p s -> PT p TLt }

&&        { \p s -> PT p TAnd }
\|\|        { \p s -> PT p TOr }
!         { \p s -> PT p TNot }

$digit+    { \p s -> PT p (TInt (read s)) }
\?$alpha[$alpha$digit\_]*  { \p s -> PT p (TVar (tail s)) }
\"[^\"]*\"  { \p s -> PT p (TString (init (tail s))) }
\<[^>]*\>   { \p s -> PT p (TURI (init (tail s))) }

.           { \p s -> error ("Lexical error at " ++ show p ++ ": unrecognised character " ++ s) }

{ 
data TokenPosn = PT AlexPosn Token deriving (Eq, Show)

-- The token type: 
data Token = 
  TSOURCE         |
  TOUTPUT         |
  TWHERE          |
  TGROUP          |
  TBY             |
  TUNION          |
  TFILTER         |
  TMAX            |
  TMIN            |
  TCOUNT          |
  TSUM            |
  TLBrace         |
  TRBrace         |
  TLParen         |
  TRParen         |
  TDot            |
  TInt Integer    |
  TVar String     |
  TString String  |
  TURI String     |
  TEq             |
  TGe             |
  TLe             |
  TNe             |
  TGt             |
  TLt             |
  TAnd            |
  TOr             |
  TNot                 
  deriving (Eq,Show) 

}
