open String
open Unix

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
    input_all_chars ch [] |> string_for_char_list;;

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
