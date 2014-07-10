type header = {name: string; value: string}
type headers = header list
type uri = string

module Parse = struct
    type request_line = string * uri * string 
    type request = request_line * headers

    type response_line = string * int * string
    type response = response_line * headers
end;;

module Request = struct
    type t = { meth: string; 
               uri: uri; 
               version: string; 
               headers: headers;
               body: string};;
end;;

module Response = struct
    type t = { version: string;
               code: int;
               reason: string;
               headers: headers;
               body: string};;
end;;
