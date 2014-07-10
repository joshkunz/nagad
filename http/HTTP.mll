{
    open HTTPTypes
    open HTTPParse
    open Printf
}

(* This is from RFC2616 *)
let CHAR = ['\000'-'\127']
let CTL = ['\000'-'\031' '\127']
let DIGIT = ['0'-'9']
let SP = ' '
let HT = '\t'
let CR = '\r'
let LF = '\n'
let CRLF = CR LF
let LWS = (CRLF)? (SP | HT)+
let TEXT = _ # CTL
let seperators = ['('  ')'  '<'  '>'  '@' ','  ';'  ':'  '\\'  '"'
                  '/'  '['  ']'  '?'  '='  '{' '}' ' ' '\009']
let token = (CHAR # CTL # seperators)+

let qdtext = TEXT # '"'
let reason_phrase = TEXT # CR # LF

let code = DIGIT DIGIT DIGIT

let http_version = ("HTTP/" DIGIT '.' DIGIT)

(* XXX: I *believe* I should be taking the quotes out of the quoted 
 * text, but I'm going to ignore that for now... *)
let field =  TEXT* | (token* | seperators* | '"' qdtext* '"')

(* Everything here pertains to RFC2396 *)
let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let hex = digit | ['A'-'F' 'a'-'f']
let mark = ['-' '_' '.' '!' '~' '*' ''' '(' ')']
let unreserved = (alpha | digit) | mark
let escaped = '%' hex hex
let pchar = unreserved | escaped | [':' '@' '&' '=' '+' '$' ',']
let segment = pchar* (';' pchar*)*
let path_segments = segment ('/' segment)*
let abs_path = '/' path_segments
(* XXX: Should also support absoluteURI, but its definition is heavyweight
 * and not applicable to our use_case. *)
let request_uri = '*' | abs_path

let status_line = (http_version as v) SP (code as c) SP (reason_phrase+ as r)
let request_line = (token as m) SP (request_uri as u) SP (http_version as v)
let header = (token as n) ':' LWS? (field as v) LWS?

rule http_token = parse 
    | status_line          { RESPONSE(v, (int_of_string c), r) }
    | request_line         { REQUEST(m, u, v) }
    | header               { HEADER({HTTPTypes.name=n; 
                                     HTTPTypes.value=v}) }
    | CRLF                 { CRLF }
    | ':'                  { COLON }
