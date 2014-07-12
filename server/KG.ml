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
    let rec remove_edge l e =
        match l with 
        | [] -> []
        | {out = o; label = la} :: l when o = e.out && la = e.label -> l
        | _e :: l -> _e :: remove_edge l e
    in
    let e = {out = f.tail; label = f.rel} in
    try
        (Graph.find g f.head |> remove_edge) e |> Graph.replace g f.head
    with Not_found -> ();;

let add_fact g f = madd_fact (Graph.copy g) f;;
let remove_fact g f = mremove_fact (Graph.copy g) f;;

let facts_off g f =
    if Graph.mem g f.head then
        Graph.find g f.head |> 
        List.map (fun e -> {head = f.head; rel = e.label; tail = e.out})
    else [];;
