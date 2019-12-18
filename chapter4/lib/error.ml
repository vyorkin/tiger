module L = Location

type t =
  | LexingError
  | SyntaxError
  [@@deriving show]

exception Error of t * L.loc * string

let error_name = function
  | LexingError -> "Lexing error"
  | SyntaxError -> "Syntax error"

let fail err l msg =
  raise @@ Error (err, L.loc l, msg)

let to_string err l msg =
  let range = L.range_string l in
  Printf.sprintf "%s:\n\t%s\n\t%s" (error_name err) range msg
