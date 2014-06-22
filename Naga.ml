open Printf
open Datalog
open DatalogParse
open DatalogLex
open Lexing
open Parsing
open List
open String

let quoted s = "\"" ^ s ^ "\"";;
let is_empty l = (List.length l) = 0;;

module Fact =
    struct
    type fact = {head: string; rel: string; tail: string}
    type fact_db = fact list

    let display_fact f = 
        print_string @@ 
        "fact(" ^ 
        f.head ^ ", " ^
        f.rel ^ ", " ^
        f.tail ^ ").\n";;

    let rec display_facts fs =
        match fs with
        | [] -> () 
        | car :: cdr ->
            display_fact car;
            display_facts cdr;;

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

let print_help () = 
    print_string @@ 
"Commands:
fact(a, b, c).      Add a fact to the database.
facts.              Display facts in the fact base.
finish. end. done.  Exit the program.
graph.              Print the DOT representation of this graph.
help.               Print this message.
help_full.          Print a much longer help message.
";;

let print_full_help () = 
    print_string @@
"The Naga REPL language is a simple Line-oriented Datalog-like language.
The system is manipulated by putting a command on each input line. A command
is of the form: 
    NAME.
or of the form:
    NAME(a [, b]*).
where the period is significant. A listing of the commands understood 
by the system is given below.

Additionally, data in the system can be retrieved through queries. A query is of
the form:
    :- command [, command]*.
These commands can be split across multiple physical lines by ending the line
with a backslash (\\). For example:
    :- foo, bar(a, b, c) \\
       baz(z, e, q).

Whitespace is not significant in the language. Queries are not currently
implemented.
";
    print_help ();;


let is_continued s =
    let t = trim s in
    get t ((length t) - 1) = '\\';;

let pop_continued s = 
    let t = trim s in
    sub t 0 ((length t) - 1);;

let try_parse s =
    try 
    let lexbuf = Lexing.from_string s in
        Some (DatalogParse.start DatalogLex.token lexbuf);
    with 
    | Parse_error -> None
    | Failure _ -> None

let fact_for_statement s = Fact.fact_for_list s.body;;

let rec frepl buf fdb =
    print_string "> ";
    let line = read_line () in
    if is_continued line then
        frepl (buf ^ (pop_continued line)) fdb
    else
    begin
    let continue = frepl "" in
    match try_parse @@ buf ^ line with
    | None -> 
        print_string "Invalid statement.\n";
        continue fdb;
    | Some (Statement s) ->
        (match handle_statement fdb s with
         | Some fdb -> continue fdb;
         | None -> fdb);
    | Some (Query q) ->
        print_string "Querying is not yet supported.\n";
        continue fdb;
    end
and handle_statement fdb s = 
    match s.head with
    | "facts" ->
        if is_empty fdb then
            print_string @@ "(empty)\n"
        else
            Fact.display_facts fdb;
        Some fdb;
    | "graph" ->
        print_string @@ (Fact.fact_graph fdb) ^ "\n";
        Some fdb;
    | "fact" ->
        let fact = fact_for_statement s in
        (match fact with
         | None ->
             print_string "That is not a valid fact.\n";
             Some fdb;
         | Some f ->
             Some (f :: fdb));
    | "help" ->
        print_help (); Some fdb;
    | "help_full" ->
        print_full_help (); Some fdb;
    | "finish" | "end" | "done" -> None;
    | o ->
        print_string @@ "That is not a valid command.\n";
        Some fdb;;

let repl () = 
    frepl "" [];;

repl();;
