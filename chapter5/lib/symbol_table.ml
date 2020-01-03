open Err
open Core_kernel

module L = Location

include Map.Make (Symbol)

let not_found name sym v =
  let msg = Printf.sprintf "Unknown %s: %s" name v in
  id_error sym msg

let find_env name env sym =
  Trace.find_env name sym;
  let key = sym.L.value in
  match find env key with
  | Some v -> v
  | None -> not_found name sym key.name

let set_env name env sym data =
  Trace.set_env name sym;
  set env ~key:sym.L.value ~data

let find_var e s = find_env "variable" e s
let find_fun e s = find_env "function" e s
let find_ty  e s = find_env "type" e s

let set_var e s v = set_env "variable" e s v
let set_fun e s v = set_env "function" e s v
let set_ty  e s v = set_env "type" e s v
