open Err
open Core_kernel

module L = Location

(* Symbol table item *)
module T = struct
  type t = {
    id : int;
    name : string;
  } [@@deriving compare, eq, sexp, show { with_path = false }]

  let mk =
    let tbl = Hashtbl.create (module String) in
    let idx = ref (-1) in
    fun key ->
      match Hashtbl.find tbl key with
      | Some id ->
        { id; name = key }
      | None ->
        incr idx;
        let data = !idx in
        Hashtbl.add_exn tbl ~key ~data;
        { id = data; name = key }
end
include T
include Comparator.Make(T)

(* Symbol table *)
module Table = struct
  include Map.Make (T)

  let not_found name sym v =
    let msg = Printf.sprintf "Unknown %s: %s" name v in
    id_error sym msg

  let find_env name sym env =
    match find env sym.L.value with
    | Some v -> v
    | None -> not_found name sym sym.L.value.name

  let find_var sym env = find_env "variable" sym env
  let find_fun sym env = find_env "function" sym env
  let find_ty  sym env = find_env "type" sym env
end
