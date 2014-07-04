open Sys
open Printf
open Lexing
open Parsing
open Common
open Datalog
open DatalogParse
open DatalogLex
open List
open Fact
open Query
open Dot

type parse_result = Empty | NoData | ParseError | Parsed of Datalog.classified;;

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

let fact_for_statement s = 
    let rec value_list = function
        | [] -> [];
        | Value(x) :: xs -> x :: value_list xs;
        | _ -> raise (Failure "fact_for_statement: Not a statement.");
    in
    Fact.fact_for_list @@ value_list s.body;;

let show_results (s : Datalog.statement) results =
    let rec textual index results = 
        match results with 
        | [] -> "End of results.\n"
        | q :: qs ->
              (sprintf "Result %i:\n" index)
            ^ (Fact.string_for_facts q)
            ^ (textual (index + 1) qs)
    in
    let node_for_node index e n =
        try 
            List.find ((=) n) e |> ignore; 
            ("", e); (* Will only be returned if List.find doesn't throw
                        an exception to the lookup *)
        with
        | Not_found -> 
            ((quoted (n ^ index)) ^ " [label=" ^ (quoted n) ^ "];\n", n :: e)
    in
    let rec nodes_for_fdb index e fdb = 
        match fdb with 
        | [] -> ""
        | f :: fs ->
            let (result1, db1) = node_for_node index e f.head in
            let (result2, db2) = node_for_node index db1 f.tail in
            result1 ^ result2 ^ (nodes_for_fdb index db2 fs)
    in
    let edge_for_fact index f =
        sprintf "\"%s%s\" -- \"%s%s\" [label=%s];\n" 
            f.head index f.tail index (quoted f.rel)
    in
    let rec subgraph index facts =
        match facts with
        | [] -> ""
        | f :: fs ->
            (edge_for_fact index f) ^ (subgraph index fs)
    in
    let rec _graph index results = 
        match results with
        | [] -> ""
        | r :: rs ->
            let rnum = (string_of_int index) in
            (sprintf "subgraph cluster%s {\n" rnum) ^
            (nodes_for_fdb rnum [] r) ^
            (subgraph rnum r) ^
            "}\n" ^ (_graph (succ index) rs)
    in
    let graph results = 
        "graph {\n" ^ (_graph 0 results) ^ "}\n"
    in
    match s.head with 
    | "text" ->
        begin
        match s.body with 
        | [] -> textual 0 results |> print_string;
        | Value (name) :: [] -> 
            let f = open_out @@ name ^ ".facts" in
            textual 0 results |> output_string f;
            close_out f
        | _ -> raise (Failure "Case not Handled.")
        end
    | "graph" ->
        begin
        match s.body with
        | [] -> graph results |> print_string
        | Value(name) :: [] ->
            let f = open_out @@ name ^ ".pdf" in
            let (status, pdf) = graph results |> Dot.pdf_for_dot in
            output_string f pdf;
            close_out f;
        | _ -> raise (Failure "Case not handled.")
        end
    | _ -> raise (Failure "Unknown query output.");;

let handle_statement fdb (s : Datalog.statement) = 
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
        print_help (); Some fdb;
    | "finish" | "end" | "exit" | "done" -> None;
    | _ ->
        print_string @@ "That is not a valid command.\n";
        Some fdb;;

let handle_query fdb handler q = 
    let item_for_value = function 
        | Value v -> Query.Value v
        | Variable v -> Query.Variable v in
    let triple_for_body = function
        | a :: b :: c :: [] -> (a, b, c)
        | _ -> raise (Failure "triple_for_body: To many items.") in
    let triple_for_statement s =
        List.map item_for_value s.body |> triple_for_body in
    let query = List.map triple_for_statement q in
    Query.query_graph query fdb |> handler;;

let eval_operation fdb operation = 
    match operation with
    | Implication (i) ->
        handle_query fdb (show_results i.implied) i.by; 
        Some fdb;
    | Query (q) ->
        handle_query fdb (show_results {head="text"; body=[]}) q;
        Some fdb;
    | Statement (s) ->
            handle_statement fdb s;;

let try_parse s =
    try 
    let parsed = Lexing.from_string s |> 
                 DatalogParse.operation 
                    (DatalogLex.token DatalogLex.throw_eof) |>
                 Datalog.classify in Parsed parsed
    with 
    | Parse_error -> 
        print_string "Got a parse error exception.\n";
        ParseError;
    | DatalogLex.Eof -> NoData;;

let rec frepl buf fdb =
    if buf = "" then
        print_string "> "
    else
        print_string "... ";
    let line = (read_line ()) ^ "\n" in
    let cbuf = buf ^ line in
    match try_parse cbuf with
    | ParseError -> 
        print_string "Invalid statement.\n";
        frepl "" fdb;
    | NoData ->
        frepl cbuf fdb;
    | Empty ->
        assert (buf = "");
        frepl "" fdb;
    | Parsed p ->
        begin
        match eval_operation fdb p with
        | Some fdb -> frepl "" fdb
        | None -> fdb
        end;;

let repl () = 
    frepl "" [];;

let parse_source ch =
    Lexing.from_channel ch |> 
    DatalogParse.program (DatalogLex.token DatalogLex.gen_eof) |>
    Datalog.classify_program

let rec eval_program program fdb = 
    match program with 
    | [] -> ()
    | o :: os ->
        begin
        match eval_operation fdb o with
        | Some fdb -> eval_program os fdb
        | None -> ()
        end;;

open Array
let main () =
    match Sys.argv with
    | [| _ |] -> repl () |> ignore
    | [| _; "-f"; n |] -> eval_program (open_in n |> parse_source) []
    | _ -> print_endline "Unrecognized flags.";;

main ();;
