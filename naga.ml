open Scanf
open Printf

module FactIO =
    struct

    type fact = {head: string; rel: string; tail: string}
    type fact_db = fact list

    let make_fact head rel tail = {head=head; rel=rel; tail=tail};;

    let prompt = "> ";;
    let err_lead = "!> ";;
    let repl_err s =
        print_string @@ err_lead ^ s;;

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

    let rec _read_facts fact_db =
        print_string prompt;
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
            repl_err @@ "\"" ^ line ^ "\"" ^
                        " is not a valid fact string, " ^
                        "it's lacking some parameters.\n";
            _read_facts fact_db;
        | Scanf.Scan_failure s ->
            repl_err @@ "\"" ^ line ^ "\"" ^
                     " is not a valid fact string.\n";
            repl_err @@ s ^ "\n";
            _read_facts fact_db;;

    let read_facts () = _read_facts [];;
    end;;

let my_facts = FactIO.read_facts () in
print_string "\nFACTS:\n\n";
FactIO.display_facts my_facts;;
