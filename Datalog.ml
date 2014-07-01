type value = Variable of string | Value of string;;
type statement = {head: string; body: value list};;
type fragment = statement list;;

type classified = Statement of statement | Query of fragment;;

let rec is_var_in_list = function
    | [] -> false;
    | Variable(x) :: xs -> true;
    | Value(x) :: xs -> is_var_in_list xs;

exception Could_not_classify_fragment

let classify = function
    | s :: [] when not (is_var_in_list s.body) -> Statement s;
    | s :: ss -> Query(s :: ss);
    | _ -> raise Could_not_classify_fragment;;
