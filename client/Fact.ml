open List
open String
open Common
open Hashtbl

(* The keys in our table are the nodes, so we use strings *)
module Graph = Hashtbl.Make(String);;
type edge = {out: string, label: string};;
type fact = {head: string, rel: string, tail: string};;

let add_fact g f =
    let e = {out: f.tail, label: f.rel} in
    if Graph.mem g f.head then
        e :: (Graph.find g f.head) |> Graph.replace g f.head
    else
        Graph.replace g f.head [e];;

let remove_fact g f =
    let remove_edge l e =
        match l with 
        | [] -> []
        | {out: o, label: la} :: l when o = e.out && la = e.label -> l
        | _e :: l -> _e :: remove_edge l e
    in
    let e = {out: f.tail, label: f.rel} in
    try
        (Graph.find g f.head |> remove_edge) e |> Graph.replace g f.head
    with Not_found -> ();;

let string_for_fact f =
    "fact(" ^ 
    f.head ^ ", " ^
    f.rel ^ ", " ^
    f.tail ^ ").\n";;

let rec string_for_facts fs = 
    match fs with
    | [] -> ""
    | car :: cdr ->
        string_for_fact car ^ string_for_facts cdr;;

let display_fact f = 
    string_for_fact f |> print_string;;

let display_facts fs =
    string_for_facts fs |> print_string;;

let edge_for_fact f =
    (quoted f.head) 
    ^ " -- " ^ 
    (quoted f.tail) ^ " [label=" ^ (quoted f.rel) ^ "];\n";;

let fact_for_list l = 
    if (List.length l) <> 3 then None else
        Some { head = List.nth l 0; 
               rel  = List.nth l 1; 
               tail = List.nth l 2};;

let rec edges_for_facts fs =
    match fs with 
    | [] -> "";
    | f :: fs ->
        (edge_for_fact f) ^ (edges_for_facts fs);;

let fact_graph fs = "graph {\n" ^ (edges_for_facts fs) ^ "}";;
