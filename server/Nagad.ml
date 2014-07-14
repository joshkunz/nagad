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
open Buffer

let graph = ref (KG.empty ());;
let g = Mutex.create ();;

(* Shorter, composable form of json encode *)
let (|@) e l = 
    match Jsonm.encode e (`Lexeme l) with
    | `Ok -> e
    | _ -> Failure("Internal JSON Encoder error.") |> raise;;

type decoded = [ 
    | `String of string
    | `Name of string
    | `Os | `Oe | `As | `Ae | `End ];;

let (!*) d : decoded = 
    match Jsonm.decode d with
    | `Lexeme `Os -> `Os
    | `Lexeme `Oe -> `Oe
    | `Lexeme `As -> `As
    | `Lexeme `Ae -> `Ae
    | `Lexeme (`String s) -> `String s
    | `Lexeme (`Name n) -> `Name n
    | `End -> `End
    | _ -> raise (Failure "Unhandled json decoded case.");;

let json_for_graph g = 
    let rec json_for_edge enc e =
        enc 
        |@ `Os
            |@ `Name "label" |@ `String e.label
            |@ `Name "to"    |@ `String e.out
        |@ `Oe 
    in
    let json_for_adj_list k v enc = 
        enc
        |@ `Name k 
            |@ `As 
                |> fun x -> List.fold_left json_for_edge x v 
            |@ `Ae
    in
    let buf = Buffer.create 100 in
    Jsonm.encoder (`Buffer buf) |@ `Os 
    |> KG.Graph.fold json_for_adj_list g |@ `Oe
    |> fun x -> Jsonm.encode x `End |> ignore;
    Buffer.contents buf;;

exception Json_not_graph

let graph_for_json j =
    let open KG in
    let rec p10 d g h r t = p4 d (KG.add_fact g {head=h; rel=r; tail=t}) h
    and p8 d g h r = match !* d with
        | `String t -> p10 d g h r t
        | _ -> raise Json_not_graph
    and p8_1 d g h t = match !* d with
        | `String r -> p10 d g h r t
        | _ -> raise Json_not_graph
    and p7 d g h r = match !* d with
        | `Name "to" -> p8 d g h r
        | _ -> raise Json_not_graph
    and p7_1 d g h t = match !* d with
        | `Name "label" -> p8_1 d g h t
        | _ -> raise Json_not_graph
    and p6 d g h = match !* d with
        | `String r -> p7 d g h r
        | _ -> raise Json_not_graph
    and p6_1 d g h = match !* d with
        | `String t -> p7_1 d g h t
        | _ -> raise Json_not_graph
    and p5 d g h = match !* d with
        | `Name "label" -> p6 d g h 
        | `Name "to" -> p6_1 d g h 
        | _ -> raise Json_not_graph
    and p4 d g h = match !* d with
        | `Os -> p5 d g h
        | `Oe -> p4 d g h
        | `Ae -> p2 d g
        | _ -> raise Json_not_graph
    and p3 d g h = match !* d with
        | `As -> p4 d g h
        | _ -> raise Json_not_graph
    and p2 d g = match !* d with
        | `Name h -> p3 d g h
        | `Oe -> p1 d g
        | _ -> raise Json_not_graph
    and p1 d g = match !* d with
        | `Os -> p2 d g
        | `End -> g
        | _ -> raise Json_not_graph
    in
    p1 (Jsonm.decoder (`String j)) (KG.empty ());;

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
        | "/query" -> Response.make 200 "Query.";
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
