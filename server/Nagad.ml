open Sys
open Printf
open Unix 
open List
open KG 
open Query
open HTTP
open Thread
open Mutex

let handle_client (ic, oc, addr) = 
    print_endline "Got new client!";;

let main port =
    let tcp = (getprotobyname "tcp").p_proto in
    let sock = socket PF_INET SOCK_STREAM tcp in
    ADDR_INET (inet_addr_any, port) |> bind sock;
    listen sock 10;

    let rec accept_loop () = 
        let (csock, addr) = accept sock in
        let (ic, oc) = (in_channel_of_descr csock, out_channel_of_descr csock) in
        Thread.create handle_client (ic, oc, addr) |> ignore;
        accept_loop () in
    accept_loop ();;

main 80;;
