open Core_kernel
open Err

module L = Location

include Map.Make (Symbol)

type entry =
  | Var
  | Fun
  | Typ

let entry_abbr = function
  | Var -> "var"
  | Fun -> "fun"
  | Typ -> "typ"

let entry_string = function
  | Var -> "variable"
  | Fun -> "function"
  | Typ -> "type"

let not_found name sym v =
  let msg = Printf.sprintf "Unknown %s: %s" name v in
  id_error sym msg

let look entry env sym =
  Trace.SymbolTable.look (entry_abbr entry) sym;
  let key = sym.L.value in
  match find env key with
  | Some v -> v
  | None -> not_found (entry_string entry) sym key.name

let bind entry env sym data =
  Trace.SymbolTable.bind (entry_abbr entry) sym;
  set env ~key:sym.L.value ~data

let look_var e s = look Var e s
let look_fun e s = look Fun e s
let look_typ e s = look Typ e s

let bind_var e s v = bind Var e s v
let bind_fun e s v = bind Fun e s v
let bind_typ e s v = bind Typ e s v
