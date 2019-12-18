open Core_kernel

type t = int * string [@@deriving show]

let symbol =
  let tbl = Hashtbl.create (module String) in
  let idx = ref (-1) in
  fun key ->
    match Hashtbl.find tbl key with
    | Some x -> x, key
    | None ->
      incr idx;
      Hashtbl.add_exn tbl ~key ~data:!idx;
      !idx, key

let name = snd
