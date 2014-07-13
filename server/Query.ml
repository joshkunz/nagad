open Printf
open List
open KG 

module Context = struct
    type entry = (string * string)
    type t = entry list

    let empty = [];;
    let binding c k = List.assoc k c;;
    let mem c k = List.mem_assoc k c;;
    let bind c k v = (k, v) :: c;;

    let rec as_string = function
        | [] -> ""
        | (k, v) :: cs -> 
            ("(" ^ k ^ ", " ^ v ^ ")\n") ^ (as_string cs);;
end;;

type query_item = Variable of string | Value of string;;
type query_triple = {head: query_item;
                      rel: query_item;
                     tail: query_item};;

type query = query_triple list;;

let qitem_as_string = function
    | Variable a -> a ^ "?"
    | Value a -> a;;

let rec qtriple_as_string = function
    | {head=a; rel=b; tail=c} ->
        "(" ^ (qitem_as_string a) ^ ", "
            ^ (qitem_as_string b) ^ ", "
            ^ (qitem_as_string c) ^ ")";;

let field_match (qf, ef) context =
    match qf with
    | Variable x ->
        if Context.mem context x then 
            (ef = (Context.binding context x), context)
        else
            (* If there is not binding for the variable in the context,
             * then automatically match and add the binding *)
            (true, Context.bind context x ef)
    | Value x -> (ef = x, context);;

(* Returns a list of tuples, the relation between fact fields
 * and query fields *)
let epairs (q : query_triple) (e : KG.fact) = 
    [(q.head, e.head); (q.rel, e.rel); (q.tail, e.tail)];;

let fact_as_string (f : KG.fact) = sprintf "fact(%s, %s, %s)" f.head f.rel f.tail;;

(* Checks to see if a given set of pairs 'is a match'. That is to say, that
 * all constants are equal, and that all variables can be bound meaningfully.
 * This function yeilds true when the pairs all match, and a context that
 * contains any bindings that were added while performing the match. *)
let is_match pairs context =
    let match_item (v, c) x = 
        let (nv, nc) = field_match x c in ((v && nv), nc);
    in
    List.fold_left match_item (true, context) pairs;;

let matches_for graph q context =
    printf "Matching with query %s and context: %s\n" 
    (qtriple_as_string q) (Context.as_string context);
    KG.all_facts graph |> List.map fact_as_string |> 
    String.concat "\n" |> printf "Graph looks like: \n%s\n";
    let facts_for_query graph query =
        match query.head with
        | Value x -> KG.facts_off graph x
        | Variable x when Context.mem context x ->
            printf "Matching bound variable case...\n";
            Context.binding context x |> KG.facts_off graph;
        (* If the head of the query is an unbound variable, then we have
         * to check against the entire graph *)
        | Variable x -> KG.all_facts graph 
    in
    let rec match_facts  = function
    | [] -> []
    | fact :: facts ->
        printf "Matching fact %s with query %s and context: %s\n" 
        (fact_as_string fact) (qtriple_as_string q) (Context.as_string context);
        let (did_match, _context) = is_match (epairs q fact) context in
        if did_match then 
            begin
            print_endline "Matched!";
            (fact, _context) :: (match_facts facts)
            end
        else 
            begin
            print_endline "Not matched...";
            (match_facts facts)
            end
    in
    let test_facts = facts_for_query graph q in
    List.map fact_as_string test_facts |>
    String.concat "\n" |> printf "Matching against facts: \n%s\n";
    test_facts |> match_facts;;

(* -> (graph, context) *)
let rec rquery_graph graph query context path = 
    Graph.length graph |> printf "Checking against graph of size: %d\n" ;
    match query with
    | [] -> [(path, context)]
    | q :: qs -> 
        matches_for graph q context |> split graph qs path
and split graph qs path facts =
    match facts with
    | [] -> []
    | (f, cntxt) :: fs ->
        (rquery_graph (KG.remove_fact graph f) qs cntxt (KG.add_fact path f)) @
        (split graph qs path fs);;

let query_graph graph query =
    rquery_graph graph query Context.empty (KG.empty ());;
