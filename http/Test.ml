open HTTP
open Printf

let req_test = 
"GET /this/is/a/path HTTP/1.1\r
Host: localhost\r
\r\n";;

let resp_test =
"HTTP/1.1 200 OK\r
This: Works\r
Content-Length: 4\r
\r\n
Boo!";;

(* Lexing.from_string req_test |> HTTPParse.request HTTPLex.http_token;; *)
let resp = Response.from_string resp_test in 
Response.body resp |> printf "Body:\n'%s'";;
