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
    Logs.debug ~src (fun m -> m ~header:"symbol" "%s %s: %s" op name (S.to_string sym))

  let bind name sym = trace "<==" name sym
  let look name sym = trace "==>" name sym
end

module Semant = struct
  open Syntax
  open Syntax.Printer

  let src = Logs.Src.create "tig.semant" ~doc:"Semantic analysis"

  let trace  f = Logs.debug ~src (fun m -> f (m ~header:"semant"))
  let trace' s = trace (fun m -> m "%s" s)

  (* List.iteri env.path ~f:(fun idx node ->
   *     let expr_str = Syntax.show_expr node.L.value in
   *     print_endline @@ sprintf "\n[%d]:\n\t%s\n" idx expr_str); *)

  let trans_prog expr =
    trace' "trans_prog"
  let trans_ty typ =
    trace @@ fun m -> m "trans_ty: %s" (print_ty typ)
  let tr_expr expr =
    trace @@ fun m -> m "tr_expr: %s" (print_expr expr.L.value)
  let tr_var var =
    trace @@ fun m -> m "tr_var: %s" (print_var var.L.value)
  let tr_simple_var sym =
    trace @@ fun m -> m "tr_simple_var: %s" (print_simple_var sym)
  let tr_field_var var field =
    trace @@ fun m -> m "tr_field_var: %s" (print_field_var var field)
  let tr_subscript_var var sub = ()
  let tr_call f args =
    trace @@ fun m -> m "tr_call: %s" (print_call f args)
  let tr_op l r op =
    trace @@ fun m -> m "tr_op: %s" (print_op l r op)
  let tr_record ty_name vfields = ()
  let tr_record_field ty name expr = ()
  let tr_seq exprs = ()
  let tr_assign var expr = ()
  let tr_cond cond t f = ()
  let tr_while expr expr = ()
  let tr_for var expr expr expr = ()
  let tr_break br loop = ()

  let tr_let decs body = ()
  let tr_array typ size init = ()

  let trans_decs decs = ()
  let trans_tys tys = ()
  let trans_funs funs = ()
  let trans_fun_head fun_dec = ()
  let trans_var var = ()

  let ret_ty ty =
    trace @@ T.(fun m -> m "<-- %s (%s)" (to_string ty) (to_string (~! ty)))

  let assert_ty ty expr = ()
  let assert_comparison expr l r = ()
  let assert_op l r = ()
  let assert_fun_body fun_dec result = ()
  let assert_init var init_ty = ()
end

let reporter ppf =
  (* [ppf] is our pretty-printing formatter
     see https://ocaml.org/learn/tutorials/format.html#Most-general-pretty-printing-using-fprintf for deatils *)
  let report src level ~over k msgf = ()
  in ()
