open Sys
open Printf
open Unix 
open List
open KG 
open Query
open HTTP
open Thread
open Mutex

(* Close the connection that backs the given streams *)
let terminate (ic, oc) =
    Unix.shutdown (descr_of_out_channel oc) SHUTDOWN_ALL;
    close_in_noerr ic;
    close_out_noerr oc;;

let handle_client (ic, oc, addr) = 
    let open Request in 
    let request = Request.read ic in
    match request.uri with
    | "/facts" -> 
        Response.make 200 "OK" "Facts." |> Response.write oc;
    | "/graph" -> 
        Response.make 200 "OK" "Graph." |> Response.write oc;
    | "/query" -> 
        Response.make 200 "OK" "Graph." |> Response.write oc;
    | _ -> Response.make 404 "Not Found" "" |> Response.write oc;
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
