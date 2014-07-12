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

(* Types shared with the HTTPBase functor interface *)
module type HTTP_IOShared = sig
    type t
    val as_string : t -> string

    val headers : t -> headers
    val version : t -> string
    val body : t -> string
end;;

module type HTTPBase_intf = sig
    type t
    val _sheaders : t -> headers -> t;;
    val _read : Lexing.lexbuf -> (int -> int -> string) -> t 

    include HTTP_IOShared with type t := t
end;;

module type HTTPBase = sig
    type t
    val read : in_channel -> t
    val write : out_channel -> t -> unit
    val from_string : string -> t 
    val set_header : t -> string -> string -> t
    val get_header : t -> string -> string 
end;;

module type HTTP_IO = sig
    type t
    include HTTP_IOShared with type t := t
    include HTTPBase with type t := t
end;;

module HTTPBase (R : HTTPBase_intf) : (HTTPBase with type t = R.t) = struct
    type t = R.t

    let get_header r n = Header.get_header (R.headers r) n;;
    let set_header r n v = 
        Header.set_header (R.headers r) n v |> R._sheaders r

    let _string_reader s pos len = 
        if len = 0 then "" else String.sub s (pos + 1) len;;

    let _channel_reader ic pos len =
        let body = "" in
        if len = 0 then "" else (really_input ic body 0 len; body);;

    let from_string s = (Lexing.from_string s |> R._read) (_string_reader s);;
    let read ic = (Lexing.from_channel ic |> R._read) (_channel_reader ic);;
    let write oc r = R.as_string r |> output_string oc;;
end;;

module RequestBase = struct
    type t = { meth: string;
               uri: uri; 
               version: string; 
               headers: headers;
               body: string};;

    let version r = r.version
    let headers r = r.headers
    let body r = r.body

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
        | Parse_error | Failure _ -> raise (Bad_request "Couldn't parse request.")
        | Header_not_found -> raise (Bad_request "No Content-Length header.")
        | End_of_file -> raise (Bad_request "Connection terminated early.");;

    let as_string r =
        sprintf "%s %s %s\r\n%s\r\n%s" 
        r.meth r.uri r.version
        (Header.string_of_headers r.headers)
        r.body;;
end;;

module ResponseBase = struct
    type t = { version: string;
               code: int;
               reason: string;
               headers: headers;
               body: string};;

    let version r = r.version
    let headers r = r.headers
    let body r = r.body

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
        | Parse_error | Failure _ -> raise (Bad_response "Couldn't parse response.")
        | Header_not_found -> raise (Bad_response "No Content-Length header.")
        | End_of_file -> raise (Bad_response "Connection terminated early.");;

    let as_string r =
        sprintf "%s %d %s\r\n%s\r\n%s" 
        r.version r.code r.reason
        (Header.string_of_headers r.headers)
        r.body;;
end;;

module Request : sig
    include HTTP_IO
    include HTTPBase with type t := t
    val meth : t -> string
    val uri : t -> uri 
end = struct
    include RequestBase
    include (HTTPBase(RequestBase) : 
        module type of HTTPBase(RequestBase) with type t := t)
    let meth r = r.meth
    let uri r = r.uri
end;;

module Response : sig
    include HTTP_IO
    include HTTPBase with type t := t
    val code : t -> int
    val reason : t -> string
end = struct
    include ResponseBase 
    include (HTTPBase(ResponseBase) : 
        module type of HTTPBase(ResponseBase) with type t := t)
    let code r = r.code
    let reason r = r.reason
end;;
