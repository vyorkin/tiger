type t = int * string [@@deriving show]

let symbol =
  let tbl = Hashtbl.create 64 in
  let idx = ref (-1) in
  fun name ->
    try
      Hashtbl.find tbl name, name
    with Not_found ->
      incr idx;
      Hashtbl.add tbl name !idx;
      !idx, name

let name = snd
