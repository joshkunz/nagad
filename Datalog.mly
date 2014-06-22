%{
    open Datalog
%}
%token LPAREN RPAREN PERIOD COMMA IMPLIES
%token <string> WORD
%token <string> VAR
%start start
%type <Datalog.fragment> start
%%

start:
    | statement PERIOD { Statement($1) }
    | IMPLIES query PERIOD { Query($2) }

statement:
    | WORD 
    { {Datalog.shead=$1; Datalog.sbody=[]} }
    | WORD LPAREN values RPAREN 
    { {Datalog.shead=$1; Datalog.sbody=$3} }

values:
    | WORD  { $1 :: [] }
    | WORD COMMA values { $1 :: $3 }

query:
    | query_statement { $1 :: [] }
    | query_statement COMMA query { $1 :: $3}

query_statement:
    | WORD
    { {Datalog.qhead=$1; Datalog.qbody=[]} }
    | WORD LPAREN query_values RPAREN
    { {Datalog.qhead=$1; Datalog.qbody=$3} }

query_values:
    | WORD { Value($1) :: [] }
    | VAR { Variable($1) :: [] }
    | WORD COMMA query_values { Value($1) :: $3 }
    | VAR COMMA query_values { Variable($1) :: $3 }


