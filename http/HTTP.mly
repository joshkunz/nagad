%{
    open HTTP;;
    open Parsing;;
    Parsing.set_trace true;;
%}

%token CRLF COLON LWS
%token<string> TOKEN FIELD
%token<HTTP.Parse.request_line> REQUEST
%token<HTTP.Parse.response_line> RESPONSE 
%token<HTTP.header> HEADER
%start request response
%type<HTTP.Parse.request> request
%type<HTTP.Parse.response> response
%%
response:
    | RESPONSE CRLF message_headers CRLF { ($1, $3) }

request:
    | REQUEST CRLF message_headers CRLF { ($1, $3) }

message_headers:
    | message_header                  { $1 :: [] }
    | message_header message_headers  { $1 :: $2 }

message_header:
    | HEADER CRLF { $1 }
%%
