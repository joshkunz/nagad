{
    open DatalogParse
    exception Unknown_token
    exception Eof
}

let LPAREN = '('
let RPAREN = ')'
let PERIOD = '.'
let COMMA = ','
let WS = [' ' '\t' '\n']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let decimal = ['0'-'9']
let punct = [^ ',' '.' '(' ')'] # WS
let WORD = lower (lower | upper | decimal | punct)*
let VAR = upper (lower | upper | decimal | punct)*

rule token = parse 
    | WORD as str { WORD(str) }
    | VAR as str  { VAR(str) }
    | LPAREN      { LPAREN }
    | RPAREN      { RPAREN }
    | PERIOD      { PERIOD }
    | COMMA       { COMMA }
    (* Whitespace matches discard the match and re-invoke the tokenizer *)
    | WS+         { token lexbuf }
    | eof         { raise Eof }
    | _           { raise Unknown_token }
