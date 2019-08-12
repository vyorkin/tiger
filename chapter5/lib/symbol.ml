type sym = int * string [@@deriving show]
type t = sym [@@deriving show]

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

module SymbolOrd = struct
  type t = sym
  let compare = Pervasives.compare
end

module Table = Map.Make(SymbolOrd)
