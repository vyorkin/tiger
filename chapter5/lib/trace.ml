module S = Symbol
module L = Location

type target =
  | Stdout
  | File of string
[@@deriving eq, show]

type source =
  | Env of target list
  | Semant of target list
[@@deriving eq, show]

(* In the [logs] lib terminology "source" defines a
   named unit of logging whose reporting level can be set independently *)

let env_src = Logs.Src.create "tig.env" ~doc:"Symbol table"
let semant_src = Logs.Src.create "tig.semant" ~doc:"Semantic analysis"

let env_trace op name sym =
  Logs.debug ~src:env_src (fun m -> m "%s %s: %s" op name (S.to_string sym))

let env_bind name sym = env_trace "<==" name sym
let env_look name sym = env_trace "==>" name sym

let reporter ppf =
  (* [ppf] is our pretty-printing formatter
     see https://ocaml.org/learn/tutorials/format.html#Most-general-pretty-printing-using-fprintf for deatils *)
  let report src level ~over k msgf = ()
  in ()
