{
    open DatalogParse
    exception Eof
}

let LPAREN = '('
let RPAREN = ')'
let PERIOD = '.'
let COMMA = ','
let IMPLIES = ":-"
let WS = [' ' '\t' '\n']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let decimal = ['0'-'9']
let punct = [^ ',' '.' '(' ')'] # WS
let WORD = (lower | upper | decimal | punct)

rule token = parse 
    | WORD+ as str { WORD(str) }
    | LPAREN      { LPAREN }
    | RPAREN      { RPAREN }
    | PERIOD      { PERIOD }
    | COMMA       { COMMA }
    | IMPLIES     { IMPLIES }
    (* Whitespace matches discard the match and re-invoke the tokenizer *)
    | WS+         { token lexbuf }
    | eof         { raise Eof }
