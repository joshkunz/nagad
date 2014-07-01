%{
    open Datalog
%}
%token LPAREN RPAREN PERIOD COMMA
%token <string> WORD
%token <string> VAR
%start start
%type <Datalog.fragment> start
%%

start: 
    | fragment PERIOD { $1 }

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
