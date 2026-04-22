{ 
module Parser where 
import Lexer 
import AST 
}

%name parseQuery 
%tokentype { TokenPosn } 
%error { parseError }
%token 
    SOURCE { PT _ TSOURCE }
    OUTPUT { PT _ TOUTPUT }
    WHERE  { PT _ TWHERE }
    GROUP  { PT _ TGROUP }
    BY     { PT _ TBY }
    UNION  { PT _ TUNION }
    FILTER { PT _ TFILTER }

    '{'    { PT _ TLBrace }
    '}'    { PT _ TRBrace }
    '('    { PT _ TLParen }
    ')'    { PT _ TRParen }
    '.'    { PT _ TDot }


    int    { PT _ (TInt $$) } 
    var    { PT _ (TVar $$) }
    string { PT _ (TString $$) }
    uri    { PT _ (TURI $$) }

    '=='   { PT _ TEq } 
    '>'    { PT _ TGt } 
    '<'    { PT _ TLt } 
    '>='   { PT _ TGe } 
    '<='   { PT _ TLe }
    '!='   { PT _ TNe }

    MAX    { PT _ TMAX }
    MIN    { PT _ TMIN }
    COUNT  { PT _ TCOUNT }
    SUM    { PT _ TSUM }

    '&&'   { PT _ TAnd }
    '||'   { PT _ TOr }
    '!'    { PT _ TNot }

%left UNION
%left '||'
%left '&&'
%right '!'
%% 

Query : SourceList Output WhereOpt { Query $1 $2 $3 }

SourceList : Source { [$1] }
        | SourceList Source { $1 ++ [$2] }

Source : SOURCE string { File $2 }

Output : OUTPUT '{' TriplePatternList '}' { Output $3 }

WhereOpt :          { Nothing }
         | WHERE Pattern GroupOpt { Just (Where $2 $3) } 

GroupOpt :          { Nothing }
         | GROUP BY var { Just $3 }

Pattern : TriplePatternList { Basic $1 }
        | Pattern UNION Pattern { Union $1 $3 }
        | FILTER '(' FilterExpr ')' '{' Pattern '}' { Filtered $6 $3 }
        | '{' Pattern '}' { $2 }

TriplePatternList : TriplePattern { [$1] }
             | TriplePatternList TriplePattern { $1 ++ [$2] }

TriplePattern : TermOrVar TermOrVar TermOrVar '.' { TriplePattern $1 $2 $3 }

TermOrVar : var { Var $1 }
          | uri { Concrete (URI $1) }
          | string { Concrete (LitString $1) }
          | int { Concrete (LitInt $1) }
          | AggExpr { Agg $1 }

AggExpr : MAX '(' var ')' { Max $3 }
        | MIN '(' var ')' { Min $3 }
        | COUNT '(' var ')' { Count $3 }
        | SUM '(' var ')' { Sum $3 }
    
FilterExpr : TermOrVar Operator TermOrVar { Comp $2 $1 $3 }
           | FilterExpr '&&' FilterExpr { And $1 $3 }
           | FilterExpr '||' FilterExpr { Or $1 $3 }
           | '!' FilterExpr { Not $2 }
           | '(' FilterExpr ')' { $2 }

Operator : '==' { Eq }
         | '!=' { Ne }
         | '<' { Lt }
         | '>' { Gt }
         | '<=' { Le }
         | '>=' { Ge }

{ 
parseError :: [TokenPosn] -> a
parseError [] = error "Parse error at end of input"
parseError (PT (AlexPn _ l c) t : _) = 
    error ("Parse error at " 
    ++ show l ++ ":" ++ show c 
    ++ ", unexpected token: " ++ show t) 
} 