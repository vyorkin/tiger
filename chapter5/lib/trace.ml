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

module Src = struct
  let symbol = Logs.Src.create "tig.symbol" ~doc:"Symbol table"
  let semant = Logs.Src.create "tig.semant" ~doc:"Semantic analysis"
end

module Symbol = struct
  let trace op name sym =
    Logs.debug ~src:Src.symbol (fun m -> m "%s %s: %s" op name (S.to_string sym))

  let bind name sym = trace "<==" name sym
  let look name sym = trace "==>" name sym
end

(* module Semant = struct
 * end *)

let reporter ppf =
  (* [ppf] is our pretty-printing formatter
     see https://ocaml.org/learn/tutorials/format.html#Most-general-pretty-printing-using-fprintf for deatils *)
  let report src level ~over k msgf = ()
  in ()
