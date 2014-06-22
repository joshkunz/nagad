type statement = {head: string; body: string list};;
type query = statement list;;
type fragment = Statement of statement | Query of query;;
