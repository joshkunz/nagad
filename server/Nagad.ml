open Sys
open Printf
open List
open KG 
open Query
open HTTP
open Thread
open Mutex;;

let g = KG.empty () in
KG.madd_fact g {head="a"; rel="b"; tail="c"};
KG.madd_fact g {head="c"; rel="b"; tail="a"};
KG.madd_fact g {head="c"; rel="b"; tail="c"};
KG.madd_fact g {head="c"; rel="b"; tail="a"};
let query = [{head=Variable "A"; rel=Variable "B"; tail=Variable "C"}; 
             {head=Variable "C"; rel=Variable "B"; tail=Value "a"}] in
let result = Query.query_graph g query in
printf "Result count: %d\n" (List.length result);

(*
let main () =
    let eval_prog = eval_program handle_query handle_statement in
    match Sys.argv with
    | [| _ |] -> repl () |> ignore
    | [| _; "-f"; n |] -> eval_prog (open_in n |> parse_source) [] |> ignore
    | _ -> print_endline "Unrecognized flags.";;

main ();;
*)
