open Printf
open String
open Hashtbl
open List

(* The keys in our table are the nodes, so we use strings *)
module Graph = Hashtbl.Make(struct
    type t = String.t;;
    let equal k1 k2 = (String.compare k1 k2) = 0;;
    let hash k = Hashtbl.hash k;;
end);;

type edge = {out: string; label: string};;
type fact = {head: string; rel: string; tail: string};;

(* Yields a new empty graph *)
let empty () = Graph.create 1;;

(** Add a fact to the given knowledge graph. *)
let madd_fact g f =
    let e = {out = f.tail; label = f.rel} in
    if Graph.mem g f.head then
        e :: (Graph.find g f.head) |> Graph.replace g f.head
    else
        Graph.replace g f.head [e];;

(** Remove the first fact that matches 'f' from the given graph. This function
  * has no effect if f is not in g. *)
let mremove_fact g f =
    let rec mremove_edge l e =
        match l with 
        | [] -> print_endline "nothing removed."; []
        | {out = o; label = la} :: l when o = e.out && la = e.label -> 
                print_endline "something removed.";
                l
        | _e :: l -> _e :: mremove_edge l e
    in
    let e = {out = f.tail; label = f.rel} in
    try
        (Graph.find g f.head |> mremove_edge) e |> Graph.replace g f.head
    with Not_found -> ();;

(* Non-mutating versions of add_fact and remove_fact *)
let add_fact g f = 
    let ng = (Graph.copy g) in
    madd_fact ng f; ng;;

let remove_fact g f = 
    let ng = (Graph.copy g) in
    mremove_fact ng f; ng;;

(* Yields a list of all facts 'f' such that there is an edge n -> f. Yields
 * an empty list if there are no such facts. *)
let facts_off g n =
    if Graph.mem g n then
        Graph.find g n |> 
        List.map (fun e -> {head = n; rel = e.label; tail = e.out})
    else [];;

(* Yields a list of all facts in the graph. *)
let all_facts g : fact list =
    let aggregator k v a = 
        (List.map (fun e -> {head=k; rel=e.label; tail=e.out}) v) @ a in
    Graph.fold aggregator g [];;
