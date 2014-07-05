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
let SDELIM = '"'
let lower = ['a'-'z']
let upper = ['A'-'Z']
(* Specials: COMMA, PERIOD, LPAREN, RPAREN, COMMENT, SDELIM, IMPL[0] *)
let specials = [',' '.' '(' ')' '#' '"' ':']
let extra = _ # WS # lower # upper # specials 
let WORD = (lower | extra) (lower | upper | extra)*
let VAR = upper (lower | upper | extra)*
let SCONSTW = _ # SDELIM # upper
let SCONSTV = upper 
let SCONST = _ # SDELIM

rule token on_eof = parse 
    | SDELIM (SCONSTW SCONST+ as str) SDELIM { WORD(str) }
    | SDELIM (SCONSTV SCONST+ as str) SDELIM { VAR(str) }
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
