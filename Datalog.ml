type statement = {shead: string; sbody: string list};;
type query_value = Variable of string | Value of string;;
type query_statement = {qhead: string; qbody: query_value list };;
type query = query_statement list;;
type fragment = Statement of statement | Query of query;;
