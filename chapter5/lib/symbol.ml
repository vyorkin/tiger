open Core_kernel

module L = Location

type t = {
  id : int;
  name : string;
} [@@deriving compare, eq, sexp, show { with_path = false }]

let (=) x y = equal x y
let (<>) x y = not (equal x y)

let next_id =
  let n = ref (-1) in
  fun () -> incr n; !n

let mk =
  let tbl = Hashtbl.create (module String) ~size:128 in
  fun name ->
    match Hashtbl.find tbl name with
    | Some id ->
      { id; name }
    | None ->
      let id = next_id () in
      Hashtbl.add_exn tbl ~key:name ~data:id;
      { id; name }

let mk_unique name =
  { name; id = next_id () }

let to_string s =
  sprintf "%s <#%d> %s"
    s.L.value.name s.L.value.id (L.range_string s.L.loc)
