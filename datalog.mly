%{
module Datalog =
    struct
        type statement = {head: string; body: string list};;
        type query = Statement list;;
        type fragment = Statement of statement | Query of query;;
    end;;
%}

%token LPAREN RPAREN PERIOD COMMA IMPLIES
%token <string> WORD
%start start
%type <Datalog.fragment> start
%%

start:
    | statement PERIOD { $1 }
    | IMPLIES query PERIOD { $2 }

query:
    | { [] }
    | statement COMMA query 
    { $1 :: $3 }

statement:
    | WORD 
    { {Datalog.head=$1; Datalog.body=[]} }
    | WORD LPAREN values RPAREN 
    { {Datalog.head=$1; Datalog.body=$3} }

values:
    | WORD  { $1 :: [] }
    | WORD COMMA values { $1 :: $3 }
