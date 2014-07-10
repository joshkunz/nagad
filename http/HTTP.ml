open Lexing
open Parsing
open HTTPLex
open HTTPParse
open HTTPTypes

open Printf
open List

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

exception Bad_request of string;;
exception Bad_response of string;;

module type HTTP_IO = sig
    type t
    val read : in_channel -> t
    val write : out_channel -> t -> unit
    val from_string : string -> t 
    val as_string : t -> string
    val set_header : t -> string -> string -> t
    val get_header : t -> string -> string 
end;;

module type HTTP_IO_Base = sig
    type t
    val _fheaders : t -> headers;;
    val _sheaders : t -> headers -> t;;
    val _read : Lexing.lexbuf -> (int -> int -> string) -> t 
    val as_string : t -> string
end;;

module HTTPBase (R : HTTP_IO_Base) : (HTTP_IO with type t = R.t) = struct
    type t = R.t
    let get_header r n = Header.get_header (R._fheaders r) n;;
    let set_header r n v = 
        Header.set_header (R._fheaders r) n v |> R._sheaders r

    let from_string s = 
        let reader pos len = String.sub s pos len in
        (Lexing.from_string s |> R._read) reader ;;
    let as_string = R.as_string

    let write oc r = as_string r |> output_string oc;;
    let read ic = 
        let body = "" in
        let reader pos len = really_input ic body 0 len; body in
        (Lexing.from_channel ic |> R._read) reader;;
end;;

module Request : HTTP_IO = HTTPBase(struct
    type t = { meth: string;
               uri: uri; 
               version: string; 
               headers: headers;
               body: string};;

    let _fheaders r = r.headers;;
    let _sheaders r h =  
        { meth = r.meth; uri = r.uri; version = r.version;
          headers = h; body = r.body };;

    let _read buf reader =
        try 
            let ((m, p, v), hdrs) = HTTPParse.request HTTPLex.http_token buf in
            let length = Header.get_header hdrs "Content-Length" |> int_of_string in
            let body = reader buf.lex_curr_p.pos_cnum length in
            { meth = m; uri = p; version = v; headers = hdrs; body = body}
        with
        | Parse_error -> raise (Bad_request "Couldn't parse request.")
        | Header_not_found -> raise (Bad_request "No Content-Length header.")
        | End_of_file -> raise (Bad_request "Connection terminated early.");;

    let as_string r =
        sprintf "%s %s %s\r\n%s\r\n%s" 
        r.meth r.uri r.version
        (Header.string_of_headers r.headers)
        r.body;;
end);;

module Response : HTTP_IO = HTTPBase(struct
    type t = { version: string;
               code: int;
               reason: string;
               headers: headers;
               body: string};;

    let _fheaders r = r.headers;;
    let _sheaders r h =
        { version = r.version; code = r.code; reason = r.reason;
          headers = h; body = r.body };;
    let _read buf reader =
        try 
            let ((v, c, r), hdrs) = HTTPParse.response HTTPLex.http_token buf in
            let length = Header.get_header hdrs "Content-Length" |> int_of_string in
            let body = reader buf.lex_curr_p.pos_cnum length in
            { version = v; code = c; reason = r; headers = hdrs; body = body}
        with
        | Parse_error -> raise (Bad_response "Couldn't parse response.")
        | Header_not_found -> raise (Bad_response "No Content-Length header.")
        | End_of_file -> raise (Bad_response "Connection terminated early.");;

    let as_string r =
        sprintf "%s %d %s\r\n%s\r\n%s" 
        r.version r.code r.reason
        (Header.string_of_headers r.headers)
        r.body;;
end);;
