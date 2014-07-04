%{
    open Datalog
%}
%token LPAREN RPAREN PERIOD COMMA IMPL EOF
%token <string> WORD
%token <string> VAR
%start operation 
%start program
%type <Datalog.operation> operation
%type <Datalog.program> program
%%

program: 
    | EOF               { [] }
    | operation program { $1 :: $2 }

single_operation:
    | operation { $1 }
    | EOF { raise Datalog.Parse_eof }

operation: 
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
