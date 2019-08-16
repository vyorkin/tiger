open Error

module L = Location

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

let id = fst
let name = snd

module SymbolOrd = struct
  type t = sym
  let compare = Pervasives.compare
end

module Table = struct
  include Map.Make(SymbolOrd)

  let find_env env_name sym env =
    match find_opt sym.L.value env with
    | Some v ->
      v
    | None ->
      id_error sym @@
      Printf.sprintf "Unknown %s: %s"
        env_name (name sym.L.value)

  let find_var sym env = find_env "variable" sym env
  let find_fun sym env = find_env "function" sym env
  let find_ty  sym env = find_env "type" sym env
end
