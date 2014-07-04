{
    open DatalogParse
    exception Eof
    exception Unknown_token

    let throw_eof () = raise Eof;;
    let gen_eof () = EOF;;
}

let COMMENT = '#'
let LPAREN = '('
let RPAREN = ')'
let PERIOD = '.'
let COMMA = ','
let IMPL = ":-"
let WS = [' ' '\t' '\n']
let specials = [',' '.' '(' ')' '#' ':']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let extra = _ # specials # WS # lower # upper
let WORD = (lower | extra) (lower | upper | extra)*
let VAR = upper (lower | upper | extra)*

rule token on_eof = parse 
    | WORD as str { WORD(str) }
    | VAR as str  { VAR(str) }
    | LPAREN      { LPAREN }
    | RPAREN      { RPAREN }
    | PERIOD      { PERIOD }
    | COMMA       { COMMA }
    | IMPL        { IMPL }
    (* Whitespace matches discard the match and re-invoke the tokenizer *)
    | WS+         { token on_eof lexbuf }
    (* Discard comments *)
    | COMMENT [^ '\n']* { token on_eof lexbuf }
    | eof         { on_eof () }
    | _           { raise Unknown_token }
