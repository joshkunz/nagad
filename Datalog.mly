%{
    open Datalog
%}
%token LPAREN RPAREN PERIOD COMMA IMPL
%token <string> WORD
%token <string> VAR
%start start
%type <Datalog.parse_result> start
%%

start: 
    | statement IMPL fragment PERIOD 
    { Implication {Datalog.implied=$1; Datalog.by=$3} }
    | fragment PERIOD { Fragment ($1) }

fragment:
    | statement { $1 :: [] }
    | statement COMMA fragment { $1 :: $3 }

statement:
    | WORD 
    { {Datalog.head=$1; Datalog.body=[]} }
    | WORD LPAREN values RPAREN 
    { {Datalog.head=$1; Datalog.body=$3} }

values: 
    | value  { $1 :: [] }
    | value COMMA values { $1 :: $3 }

value:
    | WORD { Value($1) }
    | VAR { Variable($1) }
