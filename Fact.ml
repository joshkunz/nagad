open List
open Common

type fact = {head: string; rel: string; tail: string};;
type fact_db = fact list;;

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
