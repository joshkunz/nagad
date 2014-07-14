%{
    open HTTPTypes;;
    open Parsing;;
    (* Parsing.set_trace true;; *)
%}

%token CRLF COLON LWS
%token<string> TOKEN FIELD
%token<HTTPTypes.Parse.request_line> REQUEST
%token<HTTPTypes.Parse.response_line> RESPONSE 
%token<HTTPTypes.header> HEADER
%start request response
%type<HTTPTypes.Parse.request> request
%type<HTTPTypes.Parse.response> response
%%
response:
    | RESPONSE CRLF CRLF                 { ($1, []) }
    | RESPONSE CRLF message_headers CRLF { ($1, $3) }

request:
    | REQUEST CRLF CRLF                 { ($1, []) }
    | REQUEST CRLF message_headers CRLF { ($1, $3) }

message_headers:
    | message_header                  { $1 :: [] }
    | message_header message_headers  { $1 :: $2 }

message_header:
    | HEADER CRLF { $1 }
%%
