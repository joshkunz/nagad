let WORD = ['a'-'z''A'-'Z''0'-'9' '_' '-']+
let LPAREN = '('
let RPAREN = ')'
let PERIOD = '.'
let COMMA = ','
let IMPLIES = ":-"
let WS = [' ' '\t' '\n']+

rule token = parse
    | WORD as str { WORD(str) }
    | LPAREN      { LPAREN }
    | RPAREN      { RPAREN }
    | PERIOD      { PERIOD }
    | COMMA       { COMMA }
    | IMPLIES     { IMPLIES }
    | WS          {}
