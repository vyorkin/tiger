module S = Symbol
module L = Location
module T = Type

type target =
  | Stdout
  | File of string
[@@deriving eq, show]

type source =
  | Env of target list
  | Semant of target list
[@@deriving eq, show]

(* Note that in the [logs] lib terminology "source" defines a
   named unit of logging whose reporting level can be set independently *)

module Symbol = struct
  let src = Logs.Src.create "tig.symbol" ~doc:"Symbol table"

  let trace op name sym =
    Logs.debug ~src (fun m -> m "%s %s: %s" op name (S.to_string sym))

  let bind name sym = trace "<==" name sym
  let look name sym = trace "==>" name sym
end

module Semant = struct
  let src = Logs.Src.create "tig.semant" ~doc:"Semantic analysis"

  let trace = Logs.debug ~src

  let trans_prog expr = ()
  let trans_expr expr = ()
  let trans_ty typ = ()
  let tr_expr expr = ()
  let tr_var var = ()
  let tr_simple_var sym = ()
  let tr_field_var var field = ()
  let tr_subscript_var var sub = ()
  let tr_call f args = ()
  let tr_op expr l r op = ()
  let tr_record ty_name vfields = ()
  let tr_record_field ty name expr = ()
  let tr_seq exprs = ()
  let tr_assign var expr = ()
  let tr_cond cond t f = ()
  let tr_while expr expr = ()
  let tr_for var expr expr expr = ()
  let tr_break br = ()
  let tr_let decs body = ()
  let tr_array typ size init = ()

  let trans_decs decs = ()
  let trans_dec dec = ()
  let trans_tys tys = ()
  let trans_funs funs = ()
  let trans_fun_head fun_dec = ()
  let trans_var var = ()

  let ret_int () = ()
  let ret_string () = ()
  let ret_nil () = ()
  let ret_unit () = ()

  let assert_ty ty expr = ()
  let assert_comparison expr l r = ()
  let assert_op l r = ()
  let assert_fun_body ty fun_dec = ()
  let assert_init var init_ty = ()
end

let reporter ppf =
  (* [ppf] is our pretty-printing formatter
     see https://ocaml.org/learn/tutorials/format.html#Most-general-pretty-printing-using-fprintf for deatils *)
  let report src level ~over k msgf = ()
  in ()
