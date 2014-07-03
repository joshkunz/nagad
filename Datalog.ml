type value = Variable of string | Value of string;;
type statement = {head: string; body: value list};;
type fragment = statement list;;
type implication = {implied: statement; by: fragment};;
type parse_result = Fragment of fragment | Implication of implication;;

type classified = 
    | Statement of statement
    | Query of fragment
    | Implication of implication;;

let rec is_var_in_list = function
    | [] -> false;
    | Variable(x) :: xs -> true;
    | Value(x) :: xs -> is_var_in_list xs;

exception Could_not_classify_parse

let (classify : parse_result -> classified) = function
    | Implication i -> Implication i;
    | Fragment (s :: []) when not (is_var_in_list s.body) -> Statement s;
    | Fragment (s :: ss) -> Query(s :: ss);
    | _ -> raise Could_not_classify_parse;;
