open HTTPParse
open HTTPLex
open HTTP
open Lexing

let req_test = 
"GET /this/is/a/path HTTP/1.1\r
Host: localhost\r
\r\n";;

let resp_test =
"HTTP/1.1 200 OK\r
This: Works\r
\r\n";;

Lexing.from_string req_test |> HTTPParse.request HTTPLex.http_token;;
Lexing.from_string resp_test |> HTTPParse.response HTTPLex.http_token;;
