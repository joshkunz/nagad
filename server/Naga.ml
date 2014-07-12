open Sys
open Printf
open Common
open List
open Fact
open Query
open HTTP
open Thread
open Mutex

open Array
let main () =
    let eval_prog = eval_program handle_query handle_statement in
    match Sys.argv with
    | [| _ |] -> repl () |> ignore
    | [| _; "-f"; n |] -> eval_prog (open_in n |> parse_source) [] |> ignore
    | _ -> print_endline "Unrecognized flags.";;

main ();;
