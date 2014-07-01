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
    type fact = {head: string; rel: string; tail: string}
    type fact_db = fact list

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
        print_string "Querying is not yet supported.\n";
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
        Some fdb;;

let repl () = 
    frepl "" [];;

repl();;
