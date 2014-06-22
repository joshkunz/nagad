%{
    open Datalog
%}
%token LPAREN RPAREN PERIOD COMMA IMPLIES
%token <string> WORD
%start start
%type <Datalog.fragment> start
%%

start:
    | statement PERIOD { Statement($1) }
    | IMPLIES query PERIOD { Query($2) }

query:
    | statement { $1 :: [] }
    | statement COMMA query { $1 :: $3}

statement:
    | WORD 
    { {Datalog.head=$1; Datalog.body=[]} }
    | WORD LPAREN values RPAREN 
    { {Datalog.head=$1; Datalog.body=$3} }

values:
    | WORD  { $1 :: [] }
    | WORD COMMA values { $1 :: $3 }
