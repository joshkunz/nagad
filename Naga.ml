open Printf
open Lexing
open Parsing
open Datalog
open DatalogParse
open DatalogLex
open List
open Fact
open Query
open Dot

type parse_result = NoData | ParseError | Parsed of Datalog.classified;;

let is_empty l = (List.length l) = 0;;

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

let rec query_textual index results = 
    match results with 
    | [] -> "End of results.\n"
    | q :: qs ->
          (sprintf "Result %i:\n" index)
        ^ (Fact.string_for_facts q)
        ^ (query_textual (index + 1) qs);;

let rec unique_db postfix (facts : Fact.fact_db) =
    match facts with
    | [] -> []
    | s :: ss -> 
        {Fact.head = s.head ^ postfix;
         Fact.rel = s.rel;
         Fact.tail = s.tail ^ postfix} :: unique_db postfix ss;;

let query_graph results =
    let rec query_subgraphs index res =
        match res with
        | [] -> ""
        | r :: rs ->
            let rnum = (string_of_int index) in
            (Fact.generic_fact_graph 
                (unique_db rnum r) ("subgraph " ^ "cluster" ^ rnum ^ " {\n") "}\n")
            ^ (query_subgraphs (succ index) rs) in
    "graph {\n" ^ (query_subgraphs 0 results) ^ "}\n";;

let echo_result res = query_textual 0 res |> print_string;;

let query_result (s : Datalog.statement) = 
    match s.head with 
    | "text" ->
        (match s.body with 
         | [] -> echo_result 
         | Value (name) :: [] -> 
            fun res -> 
                let f = open_out @@ name ^ ".facts" in
                query_textual 0 res |> output_string f;
                close_out f
         | _ -> raise (Failure "Case not Handled."))
    | "graph" ->
        (match s.body with
         | [] -> (fun res -> query_graph res |> print_string)
         | Value(name) :: [] ->
            fun res ->
                let f = open_out @@ name ^ ".pdf" in
                let (status, pdf) = Dot.pdf_for_dot @@ query_graph res in
                output_string f pdf;
                close_out f;
         | _ -> raise (Failure "Case not handled."))
    | _ -> raise (Failure "Unknown implication.");;

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
    | Parsed Implication(i) ->
        handle_query fdb i.by (query_result i.implied);
        frepl "" fdb;
    | Parsed Query(q) ->
        handle_query fdb q echo_result;
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
            let (status, pdf) = Dot.pdf_for_dot @@ Fact.fact_graph fdb in
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
and handle_query fdb q handler = 
    let item_for_value = function 
        | Value v -> Query.Value v
        | Variable v -> Query.Variable v in
    let triple_for_body = function
        | a :: b :: c :: [] -> (a, b, c)
        | _ -> raise (Failure "triple_for_body: To many items.") in
    let mapper s =
        triple_for_body @@ List.map item_for_value s.body in
    let query = List.map mapper q in
    Query.query_graph query fdb |> handler;;

let repl () = 
    frepl "" [];;

repl();;
