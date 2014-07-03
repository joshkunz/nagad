open Printf
open Datalog
open DatalogParse
open DatalogLex
open Lexing
open Parsing
open List
open String
open Unix

type parse_result = NoData | ParseError | Parsed of Datalog.classified;;

let quoted s = "\"" ^ s ^ "\"";;
let is_empty l = (List.length l) = 0;;

module Fact =
    struct
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
        f.head ^ " -- " ^ f.tail ^ " [label=" ^ (quoted f.rel) ^ "];\n";;

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

    let fact_graph fs = 
        "graph {\n" ^
        (edges_for_facts fs) ^
        "}";;

    end;;


module Query : sig
    open Fact
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
    end = 
    struct
    open Fact
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


let string_for_char_list cs =
    let s = String.create (List.length cs) in
    let rec fills s cl i =
        match cl with
        | [] -> s;
        | c :: cs ->
            s.[i - 1] <- c;
            (* Write the characters bacwards because they're reversed *)
            fills s cs (i - 1); in
    fills s cs (List.length cs);;

let input_string ch =
    let rec input_all_chars ch buf =
        try
            let c = input_char ch in
            input_all_chars ch (c :: buf);
        with End_of_file -> buf in
    string_for_char_list @@ input_all_chars ch [];;

exception Terminated_abnormally

let pdf_for_dot graph =
    let (proc_out, proc_in) = Unix.open_process "dot -Tpdf" in
    output_string proc_in graph;
    flush proc_in;
    close_out proc_in;
    let pdf = input_string proc_out in
    match Unix.close_process (proc_out, proc_in) with
    | WEXITED i -> (i, pdf)
    | _ -> raise Terminated_abnormally;;

let print_help () = 
    print_string @@ 
"Commands:
fact(a, b, c).      Add a fact to the database.
facts.              Display facts in the fact base.
facts(name).        Write a list of the facts in the fact base to a file
                    named 'name.facts', any files with the same name are
                    overwritten.
graph.              Print out the DOT representation of this graph.
graph(name).        Write out a PDF of the knowledge graph to a file named
                    'name.pdf'. Overwrites any file with that name in this
                    directory.
finish. end. done.  
    exit.           Exits the program
help.               Print this message.
help(full).         Print a much longer help message.
";;

let print_full_help () = 
    print_string @@
"The Naga REPL language is a simple Line-oriented Datalog-like language.
The Datalog language is as follows:

    statement = NAME. | NAME(values).
    values = variable [, values] | value [, values]
    variable = *starts with uppercase*
    value = *starts with lowercase*

The system is manipulated through commands, as listing of which is given below.
Commands are expressed as basic datatlog statements that provide a specific
function. The facts in the system (inserted with the 'fact' command) can be
queried by specifying a statement with at least one variable. Additionaly,
AND constraints can be placed on the query by seperating statements with
a comma.
";
    print_help ();;

let try_parse s =
    try 
    let lexbuf = Lexing.from_string s in
        Parsed (Datalog.classify (DatalogParse.start DatalogLex.token lexbuf));
    with 
    | Parse_error -> 
        print_string "Got a parse error exception.\n";
        ParseError;
    | DatalogLex.Eof -> NoData;;

let fact_for_statement s = 
    let rec value_list = function
        | [] -> [];
        | Value(x) :: xs -> x :: value_list xs;
        | _ -> raise (Failure "fact_for_statement: Not a statement.");
    in
    Fact.fact_for_list @@ value_list s.body;;

let rec frepl buf fdb =
    if buf = "" then
        print_string "> "
    else
        print_string "... ";
    let line = read_line () in
    let cbuf = buf ^ line in
    match try_parse cbuf with
    | ParseError -> 
        print_string "Invalid statement.\n";
        frepl "" fdb;
    | NoData ->
        frepl cbuf fdb;
    | Parsed Query(q) ->
        handle_query fdb q;
        frepl "" fdb;
    | Parsed Statement(s) ->
        (match handle_statement fdb s with
         | Some fdb -> frepl "" fdb;
         | None -> fdb);
and handle_statement fdb s = 
    match s.head with
    | "facts" ->
        begin
        match s.body with
        | [] when is_empty fdb ->
            print_string @@ "(empty)\n";
        | [] -> Fact.display_facts fdb;
        | Value(name) :: [] -> 
            let f = open_out @@ name ^ ".facts" in
            Fact.string_for_facts fdb |> output_string f;
            close_out f;
        | _ ->
            print_endline "Facts statement has too many parameters.";
        end;
        Some fdb;
    | "graph" ->
        begin
        match s.body with
        | [] -> 
            print_string @@ (Fact.fact_graph fdb) ^ "\n";
        | Value(name) :: [] ->
            let f = open_out @@ name ^ ".pdf" in
            let (status, pdf) = pdf_for_dot @@ Fact.fact_graph fdb in
            output_string f pdf;
            close_out f;
        | _ ->
            print_endline "Graph statement has too many parameters."
        end;
        Some fdb;
    | "fact" ->
        let fact = fact_for_statement s in
        (match fact with
         | None ->
             print_string "That is not a valid fact.\n";
             Some fdb;
         | Some f ->
             Some (f :: fdb));
    | "help" | "commands" ->
        begin
        match s.body with
        | [] -> print_help (); 
        | Value("full") :: [] -> print_full_help ();
        | _ ->
            print_endline "Help statement has too many, or an unknown parameter.";
        end;
        Some fdb;
    | "finish" | "end" | "done" -> None;
    | _ ->
        print_string @@ "That is not a valid command.\n";
        Some fdb
and handle_query fdb q = 
    let item_for_value = function 
        | Value v -> Query.Value v
        | Variable v -> Query.Variable v in
    let triple_for_body = function
        | a :: b :: c :: [] -> (a, b, c)
        | _ -> raise (Failure "triple_for_body: To many items.") in
    let mapper s =
        triple_for_body @@ List.map item_for_value s.body in
    let query = List.map mapper q in

    let rec query_printer index results = 
        match results with 
        | [] -> print_string "End of results.\n"
        | q :: qs ->
            printf "Result %i:\n" index;
            Fact.display_facts q;
            query_printer (index + 1) qs in
    Query.query_graph query fdb |> query_printer 1;;

let repl () = 
    frepl "" [];;

repl();;
