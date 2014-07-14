open Lexing
open Parsing
open HTTPLex
open HTTPParse
open HTTPTypes
open Buffer

open Printf
open List

type uri = string
exception Header_not_found

module Header : sig
    val get_header : headers -> string -> string 
    val set_header : headers -> string -> string -> headers
    val del_header : headers -> string -> headers

    val string_of_header : header -> string
    val string_of_headers : headers -> string
end = struct
    let rec set_header (hs : headers) n v =
        match hs with
        | {name = n_; value = v_ } :: hs_ when n = n_ ->
            {name = n; value = v} :: hs_;
        | h :: hs_ -> h :: (set_header hs_ n v)
        | [] -> {name = n; value = v} :: [];;

    let rec get_header hs n =
        match hs with 
        | {name = n_; value = v} :: hs_ when n_ = n -> v
        | h :: hs_ -> (get_header hs_ n)
        | [] -> raise Header_not_found;;

    let rec del_header hs n =
        match hs with 
        | {name = n_; value = _} :: hs_ when n = n_ -> hs_
        | h :: hs_ -> h :: (del_header hs_ n)
        | [] -> raise Header_not_found;;

    let string_of_header h = 
        sprintf "%s: %s\r\n" h.name h.value;;

    let string_of_headers hs = 
        List.map string_of_header hs |> String.concat "";;

end;;

let http_1_1 = "HTTP/1.1";;

let read_body hdrs ic buf =
    try 
        let len = Header.get_header hdrs "Content-Length" |> int_of_string in
        let body = String.create len in
        let body_in_buffer = buf.lex_buffer_len - buf.lex_curr_pos in
        if body_in_buffer > 0 then begin
            String.blit buf.lex_buffer buf.lex_curr_pos body 0 body_in_buffer;
            if len - body_in_buffer > 0 then
                really_input ic body body_in_buffer (len - body_in_buffer);
        end; body;
    with
    | Header_not_found -> "";;

exception Bad_request of string;;
exception Bad_response of string;;

module type HTTP_COMMON = sig
    type t
    val read : in_channel -> t
    val write : out_channel -> t -> unit
    val as_string : t -> string

    val set_header : t -> string -> string -> t
    val get_header : t -> string -> string 
end;;

module Request : sig
    type t = { meth: string;
               uri: uri; 
               version: string; 
               headers: headers;
               body: string};;
    include HTTP_COMMON with type t := t
    val make : string -> uri -> string -> t
end = struct
    type t = { meth: string;
               uri: uri; 
               version: string; 
               headers: headers;
               body: string};;
    let make meth uri body = 
        { meth = meth; uri = uri; version = http_1_1;
          headers = []; body = body };;

    let sheaders r h =
        { meth = r.meth; uri = r.uri; version = r.version;
          headers = h; body = r.body };;

    let get_header r n = 
        Header.get_header r.headers n;;

    let set_header r n v = 
        Header.set_header r.headers n v |> sheaders r

    let read ic = 
        let buf = Lexing.from_channel ic in
        try 
            let ((m, p, v), hdrs) = HTTPParse.request HTTPLex.http_token buf in
            { meth = m; uri = p; version = v; headers = hdrs; 
              body = read_body hdrs ic buf}
        with
        | Parse_error | Failure _ -> raise (Bad_request "Couldn't parse request.")
        | Header_not_found -> raise (Bad_request "No Content-Length header.")
        | End_of_file -> raise (Bad_request "Connection terminated early.");;

    let as_string r =
        sprintf "%s %s %s\r\n%s\r\n%s" 
        r.meth r.uri r.version
        (Header.string_of_headers r.headers)
        r.body;;

    let write oc r = as_string r |> output_string oc; flush oc;;
end;;

module Response : sig
    type t = { version: string;
               code: int;
               reason: string;
               headers: headers;
               body: string};;
    include HTTP_COMMON with type t := t
    val make : int -> string -> t
    val reason_for_code : int -> string
end = struct
    type t = { version: string;
               code: int;
               reason: string;
               headers: headers;
               body: string};;

    let codes = [
          (101, "Switching Protocols")
        ; (200, "OK")
        ; (201, "Created")
        ; (202, "Accepted")
        ; (203, "Non-Authoritative Information")
        ; (204, "No Content")
        ; (205, "Reset Content")
        ; (206, "Partial Content")
        ; (300, "Multiple Choices")
        ; (301, "Moved Permanently")
        ; (302, "Found")
        ; (303, "See Other")
        ; (304, "Not Modified")
        ; (305, "Use Proxy")
        ; (307, "Temporary Redirect")
        ; (400, "Bad Request")
        ; (401, "Unauthorized")
        ; (402, "Payment Required")
        ; (403, "Forbidden")
        ; (404, "Not Found")
        ; (405, "Method Not Allowed")
        ; (406, "Not Acceptable")
        ; (407, "Proxy Authentication Required")
        ; (408, "Request Timeout")
        ; (409, "Conflict")
        ; (410, "Gone")
        ; (411, "Length Required")
        ; (412, "Precondition Failed")
        ; (413, "Request Entity Too Large")
        ; (414, "Request-URI Too Long")
        ; (415, "Unsupported Media Type")
        ; (416, "Requested Range Not Satisfiable")
        ; (417, "Expectation Failed")
        ; (500, "Internal Server Error")
        ; (501, "Not Implemented")
        ; (502, "Bad Gateway")
        ; (503, "Service Unavailable")
        ; (504, "Gateway Timeout")
        ; (505, "HTTP Version Not Supported") ];;

    let reason_for_code c = List.assoc c codes;;

    let make code body = 
        { code = code; reason = reason_for_code code; version = http_1_1;
          headers = []; body = body };;

    let sheaders r h =
        { version = r.version; code = r.code; reason = r.reason;
          headers = h; body = r.body };;

    let get_header r n = 
        Header.get_header r.headers n;;
    let set_header r n v = 
        Header.set_header r.headers n v |> sheaders r

    let read ic =
        let buf = Lexing.from_channel ic in
        try 
            let ((v, c, r), hdrs) = HTTPParse.response HTTPLex.http_token buf in
            { version = v; code = c; reason = r; headers = hdrs; 
              body = read_body hdrs ic buf}
        with
        | Parse_error | Failure _ -> raise (Bad_response "Couldn't parse response.")
        | Header_not_found -> raise (Bad_response "No Content-Length header.")
        | End_of_file -> raise (Bad_response "Connection terminated early.");;

    let as_string r =
        sprintf "%s %d %s\r\n%s\r\n%s" 
        r.version r.code r.reason
        (if (String.length r.body) > 0 then
            set_header r "Content-Length" (String.length r.body |>
            string_of_int) |> fun x -> Header.string_of_headers x.headers
        else
            Header.string_of_headers r.headers)
        r.body;;

    let write oc r = as_string r |> output_string oc; flush oc;;
end;;
