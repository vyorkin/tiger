open Core_kernel

module L = Location

type t = {
  id : int;
  name : string;
} [@@deriving compare, eq, sexp, show { with_path = false }]

let (=) x y = equal x y
let (<>) x y = not (equal x y)

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

let to_string s =
  sprintf "%s [#%d] %s"
    s.L.value.name s.L.value.id (L.range_string s.L.loc)
