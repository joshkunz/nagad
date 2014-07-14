open Jsonm
open Printf

exception Json_decode_error
exception Json_encode_error

type cannonical_json =
    | Object of (string * cannonical_json) list
    | Array of cannonical_json list
    | String of string
    | Bool of bool
    | Float of float 
    | Null;;

let string_for_lexeme = function
    | `Os -> "`Os"
    | `Oe -> "`Oe"
    | `As -> "`As"
    | `Ae -> "`Ae"
    | `Name s -> sprintf "`Name('%s')" s 
    | `String s -> sprintf "`String('%s')" s
    | `Bool b -> sprintf "`Bool(%s)" (if b then "true" else "false")
    | `Float f -> sprintf "`Float(%f)" f
    | `Null -> "`Null"
    | _ -> raise (Failure "Unknown Lexeme");;

(* Shorter, composable form of json encode *)
let (|@) e l = 
    match Jsonm.encode e (`Lexeme l) with
    | `Ok -> e
    | _ -> Failure("Internal JSON Encoder error.") |> raise;;

let cvalue_for_value = function
    | `String s -> String s
    | `Bool b -> Bool b
    | `Float f -> Float f
    | `Null -> Null
    | _ -> raise Json_decode_error;;

let value_for_cvalue = function
    | String s -> `String s
    | Bool b -> `Bool b
    | Float f -> `Float f
    | Null -> `Null
    (* Warning, this may be caused by a faulty rule in decannonize that's
     * sending valid cvalues that aren't terminals. *)
    | _ -> raise Json_encode_error;;

let cannonize d =
    let rec pobj_v d l k = match decode d with
        | `Lexeme `As -> (k, (parr d [])) :: l |> pobj d
        | `Lexeme `Os -> (k, (pobj d [])) :: l |> pobj d
        | `Lexeme le -> (k, (cvalue_for_value le)) :: l |> pobj d
        | _ -> raise Json_decode_error
    and pobj d l = match decode d with
        | `Lexeme `Oe -> Object l
        | `Lexeme (`Name k) -> pobj_v d l k
        | _ -> raise Json_decode_error
    and parr d l = match decode d with
        | `Lexeme `Os -> (pobj d []) :: l |> parr d
        | `Lexeme `As -> (parr d []) :: l |> parr d
        | `Lexeme `Ae -> Array l
        | `Lexeme le -> (cvalue_for_value le) :: l |> parr d
        | _ -> raise Json_decode_error
    and _end d f = match decode d with
        | `End -> f
        | _ -> raise Json_decode_error
    and start d = match decode d with
        | `Lexeme `As -> parr d [] |> _end d
        | `Lexeme `Os -> pobj d [] |> _end d
        | _ -> raise Json_decode_error in
    start d;;

let decannonize enc j =
    let rec serialize enc = function
        | Object o -> enc
            |@ `Os
                |> fun e -> List.fold_left serialize_keys e o
            |@ `Oe;
        | Array a -> enc
            |@ `As 
                |> fun x -> List.fold_left serialize x a
            |@ `Ae;
        | l -> enc |@ value_for_cvalue l;
    and serialize_keys e (k, v) = serialize (e |@ `Name k) v
    in
    Jsonm.encode (serialize enc j) `End;;
