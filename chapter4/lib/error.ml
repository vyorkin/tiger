type t =
  | LexingError
  | SyntaxError
  [@@deriving show]

exception Error of t * Location.loc * string

let error_name = function
  | LexingError -> "Lexing error"
  | SyntaxError -> "Syntax error"

let fail err loc msg =
  raise @@ Error (err, Location.loc loc, msg)

let to_string err loc msg =
  let range = Location.range_string loc in
  Printf.sprintf "%s:\n\t%s\n\t%s" (error_name err) range msg
