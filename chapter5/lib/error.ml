module L = Location

type t =
  | SyntaxError
  | TypeError
  | IdError
  [@@deriving show]

exception Error of t * L.loc * string

let error_name = function
  | SyntaxError -> "Syntax"
  | TypeError   -> "Type"
  | IdError     -> "Identifier"

let fail err l msg =
  raise @@ Error (err, l.L.loc, msg)

let syntax_error l msg = fail SyntaxError l msg
let type_error l msg   = fail TypeError l msg
let id_error l msg     = fail IdError l msg

let to_string err loc msg =
  let name = error_name err in
  let range = L.range_string loc in
  Printf.sprintf "%s error:\n%s\n%s" name range msg
