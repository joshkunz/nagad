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

(* Yields a new list that contains all edges in 'l' except 'e' *)
let rec remove_edge l e =
    match l with 
    | [] -> []
    | _e :: l when _e.out = e.out && _e.label = e.label -> l
    | _e :: l -> _e :: remove_edge l e;;

(* Yields a new list that contains all edges in 'l' plus 'e' *)
let rec update_edge l e =
    match l with 
    | [] -> [e]
    | _e :: l when _e.out = e.out && _e.label = e.label -> _e :: l
    | _e :: l -> _e :: update_edge l e;;

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

(** Add a fact to the given knowledge graph. *)
let madd_fact g f =
    let e = {out = f.tail; label = f.rel} in
    if Graph.mem g f.head then
        (update_edge (Graph.find g f.head) e) |> Graph.replace g f.head
    else
        Graph.replace g f.head [e];;

(** Remove the first fact that matches 'f' from the given graph. This function
  * has no effect if f is not in g. *)
let mremove_fact g f =
    let e = {out = f.tail; label = f.rel} in
    try
        (Graph.find g f.head |> remove_edge) e |> Graph.replace g f.head
    with Not_found -> ();;

(* Modify the graph on the left hand side to reflect all edges that are
 * in the graph on the right hand side *)
let mjoin_graph_left g1 g2 = 
    all_facts g2 |> List.iter (fun f -> madd_fact g1 f);;

(* Returns a new graph that contains all facts in 'g' plus the new fact 'f' *)
let add_fact g f = 
    let ng = Graph.copy g in
    madd_fact ng f; ng;;

(* Returns a new graph that contains all facts in 'g' except 'f' *)
let remove_fact g f = 
    let ng = Graph.copy g in
    mremove_fact ng f; ng;;

(* Returns a new graph that contains the union of graph g1 and g2 *)
let join_graph g1 g2 =
    let ng = Graph.copy g1 in
    mjoin_graph_left ng g1;
    mjoin_graph_left ng g2;
    ng;;
