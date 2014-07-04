{
    open DatalogParse
    exception Unknown_token
}

let LPAREN = '('
let RPAREN = ')'
let PERIOD = '.'
let COMMA = ','
let IMPL = ":-"
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
    | IMPL        { IMPL }
    (* Whitespace matches discard the match and re-invoke the tokenizer *)
    | WS+         { token lexbuf }
    | eof         { EOF }
    | _           { raise Unknown_token }
