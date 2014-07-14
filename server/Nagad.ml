open Sys
open Printf
open Unix 
open List
open KG 
open Query
open HTTP
open Thread
open Mutex
open Jsonm
open JsonExt
open Buffer

let graph = ref (KG.empty ());;
let g = Mutex.create ();;

let json_for_graph g = 
    let rec json_for_edge e =
        Object [
             ("label", String e.label)
            ;("to", String e.out)] in
    let json_for_adj_list k v l = 
        (k, Array (List.map json_for_edge v)) :: l in
    let buf = Buffer.create 100 in
    let enc = Jsonm.encoder (`Buffer buf) in
    Object (KG.Graph.fold json_for_adj_list g []) 
        |> JsonExt.decannonize enc |> ignore;
    Buffer.contents buf;;

let graph_for_json j =
    let parse_edge g k = function 
        | Object [
             ("label", String l)
            ;("to", String t) ] ->
                {KG.head = k; KG.rel = l; KG.tail = t} |> KG.madd_fact g
        | _ -> raise JsonExt.Json_decode_error in
    let parse_adj g = function
        | (k, Array adj) -> List.iter (parse_edge g k) adj
        | _ -> raise JsonExt.Json_decode_error in
    let parse_graph g = function
        | Object adj_lists -> List.iter (parse_adj g) adj_lists
        | _ -> raise JsonExt.Json_decode_error in
    let dec = Jsonm.decoder (`String j) in
    let graph = KG.empty () in
    JsonExt.cannonize dec |> parse_graph graph; graph;;

let is_titlecase s = 
    String.length s > 0 
    && 'A' <= s.[0] && s.[0] <= 'Z';;

let query_for_json j =
    let parse_item i = 
        if is_titlecase i then Variable i else Value i in
    let parse_triple = function 
        | Array [String i1; String i2; String i3] ->
            {head = parse_item i1; 
              rel = parse_item i2; 
             tail = parse_item i3}
        | _ -> raise JsonExt.Json_decode_error in
    let parse_query = function
        | Array triples -> List.map parse_triple triples
        | _ -> raise JsonExt.Json_decode_error in
    let dec = Jsonm.decoder (`String j) in
    JsonExt.cannonize dec |> parse_query;;

(* Close the connection that backs the given streams *)
let terminate (ic, oc) =
    Unix.shutdown (descr_of_out_channel oc) SHUTDOWN_ALL;
    close_in_noerr ic;
    close_out_noerr oc;;

(* Run the function 'f' over the data-structure 'a' under the lock 'l'.
 * It ensures that the code is always run under with a locked data-structure
 * and that the structure is always unlocked after the function exits. *)
let sync f l a = 
    Mutex.lock l;
    try 
        let out = f a in
        Mutex.unlock l; out;
    with
    | x -> Mutex.unlock l; raise x;;

let handle_client (ic, oc, addr) = 
    let open Request in 
    let handle_request request = 
        match request.uri with
        | "/graph" -> 
            begin match request.meth with
            | "GET" -> 
                sync (fun _ -> json_for_graph !graph) g () 
                    |> Response.make 200;
            | "POST" ->
                graph_for_json request.body
                    |> sync (fun ug -> mjoin_graph_left !graph ug) g;
                Response.make 200 "";
            | _ -> Response.make 405 ""
            end;
        | _ -> Response.make 404 "";
    in begin try
        Request.read ic |> handle_request |> Response.write oc;
    with
        (* | x -> Response.make 500 "" |> Response.write oc; *)
        | x -> raise x 
    end;
    terminate (ic, oc);;

let main port =
    let tcp = (getprotobyname "tcp").p_proto in
    let sock = socket PF_INET SOCK_STREAM tcp in
    setsockopt sock SO_REUSEADDR true;
    ADDR_INET (inet_addr_any, port) |> bind sock;
    listen sock 10;

    let rec accept_loop () = 
        let (csock, addr) = accept sock in
        let (ic, oc) = (in_channel_of_descr csock, out_channel_of_descr csock) in
        Thread.create handle_client (ic, oc, addr) |> ignore;
        accept_loop () in
    accept_loop ();;

main 8080;;
