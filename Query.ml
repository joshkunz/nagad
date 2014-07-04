open Fact

module Query : sig

type query_item = Variable of string | Value of string
type qtri = query_item * query_item * query_item
type query = qtri list

type context_entry = string * string
type context = context_entry list

val in_context : string -> context -> bool
val edge_pairs : qtri -> Fact.fact -> (query_item * string) list
val field_match : (query_item * string) -> context -> (bool * context)
val edge_pairs_matched : (query_item * string) list -> context -> (bool * context)
val matches_of : qtri -> Fact.fact_db -> context -> (Fact.fact * context) list
val query_graph : query -> Fact.fact_db -> Fact.fact_db list

end =  struct

type query_item = Variable of string | Value of string
type qtri = query_item * query_item * query_item
type query = qtri list

type context_entry = string * string
type context = context_entry list

let rec context_as_string = function 
    | [] -> ""
    | (c1, c2) :: cs -> 
        ("(" ^ c1 ^ ", " ^ c2 ^ ")\n") ^ (context_as_string cs);;

let qitem_as_string = function
    | Variable a -> a ^ "?"
    | Value a -> a;;

let rec qtri_as_string = function
    | (a, b, c) ->
        "(" ^ (qitem_as_string a) ^ ", "
            ^ (qitem_as_string b) ^ ", "
            ^ (qitem_as_string c) ^ ")";;

let in_context v context =
    try 
        List.assoc v context |> ignore; true;
    with
    | Not_found -> false;;

let rec pop_edge graph edge =
    match graph with 
    | [] -> [] 
    | e :: es when e = edge -> es
    | e :: es -> e :: (pop_edge es edge);;

let edge_pairs (q1, q2, q3) (e : Fact.fact) = 
    [(q1, e.head); (q2, e.rel); (q3, e.tail)];;

let field_match (qfield, efield) context =
    match qfield with
    | Variable x ->
        if in_context x context then 
            (efield = (List.assoc x context), context)
        else
            (* If there is not binding for the variable in the context,
             * then automatically match and add the binding *)
            (true, (x, efield) :: context)
    | Value x -> (efield = x, context);;

let edge_pairs_matched pairs context =
    let reducer (v, c) x = 
        let (nv, nc) = field_match x c in ((v && nv), nc);
    in
    List.fold_left reducer (true, context) pairs;;

let rec matches_of qt kgraph context =
    match kgraph with
    | [] -> []
    | fact :: facts ->
        let (did_match, _context) = 
            edge_pairs_matched (edge_pairs qt fact) context
        in
        if did_match then 
            (fact, _context) :: (matches_of qt facts context)
        else
            (matches_of qt facts context);;

let rec query_tree query kgraph context path = 
    match query with
    | [] -> [path]
    | q :: qs -> 
        matches_of q kgraph context |> mapping qs kgraph path
and mapping qs kgraph path edges =
    match edges with
    | [] -> []
    | (e, cntxt) :: es ->
        let fpath = 
            query_tree qs (pop_edge kgraph e) cntxt (e :: path)
        in
        fpath @ (mapping qs kgraph path es)

let query_graph query kgraph =
    query_tree query kgraph [] [];;

end;;
