open Scanf
open Printf

let quoted s = "\"" ^ s ^ "\"";;
let is_empty l = (List.length l) == 0;;

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

module type REPL_TYPE =
    sig
    val prompt: string
    val err_lead: string
    end;;

module FactParser =
    functor (R: REPL_TYPE) -> 
        struct

    let repl_err s =
        print_string @@ R.err_lead ^ s;;

    let make_fact head rel tail = { Fact.head=head; 
                                    Fact.rel=rel; 
                                    Fact.tail=tail};;

    let rec _read_facts fact_db =
        print_string R.prompt;
        let line = read_line () in
        try
            if (String.compare line "finish.") == 0 then
                fact_db
            else
                _read_facts @@ 
                    ((Scanf.sscanf line "fact(%s@, %s@, %s@)." make_fact) 
                     :: fact_db);
        with 
        | End_of_file ->
            repl_err @@ (quoted line) ^
                        " is not a valid fact string, " ^
                        "it's lacking some parameters.\n";
            _read_facts fact_db;
        | Scanf.Scan_failure s ->
            repl_err @@ (quoted line) ^
                     " is not a valid fact string.\n";
            repl_err @@ s ^ "\n";
            _read_facts fact_db;;

    let read_facts () = _read_facts [];;
    end;;

module NagaFactParser = FactParser(struct
    let prompt = "fact> ";;
    let err_lead = "fact! ";;
end);;

let rec frepl fdb =
    print_string "> ";
    let line = read_line () in
    match line with
    | "add_facts." ->
            frepl @@ NagaFactParser.read_facts () @ fdb;
    | "facts." ->
            if is_empty fdb then
                print_string @@ "(empty)\n"
            else
                Fact.display_facts fdb;
            frepl fdb;
    | "graph." ->
            print_string @@ Fact.fact_graph fdb;
            frepl fdb;
    | "query." ->
            print_string "Quering is not yet supported.\n";
            frepl fdb;
    | "help." ->
            print_string @@ "Commands: \n" ^
                            "add_facts.    Add new facts to the fact base.\n" ^
                            "facts.        Display facts in the fact base.\n" ^
                            "query.        Write and execute a new query.\n" ^
                            "finish.       Exit the program.\n"^
                            "help.         This message\n";
            frepl fdb;
    | "finish." -> ();
    | s -> 
            print_string @@ (quoted line) ^ " is not a valid command, type " ^
                            (quoted "finish.") ^ " to exit\n";
            frepl fdb;;

let repl () = frepl [];;
repl();;
