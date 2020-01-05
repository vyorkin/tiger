open Core_kernel

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
    Logs.debug ~src (fun m ->
        m ~header:"symbol" "%s %s: %s" op name (S.to_string sym))

  let bind name sym = trace "<==" name sym
  let look name sym = trace "==>" name sym
end

module Semant = struct
  open Syntax
  open Syntax.Printer

  let src = Logs.Src.create "tig.semant" ~doc:"Semantic analysis"
  let trace  f = Logs.debug ~src (fun m -> f (m ~header:"semant"))
  let trace' s = trace (fun m -> m s)
  let print_tr name expr = trace @@ fun m -> m ">>> %s: %s" name expr

  let trans_prog _ =
    trace' "trans_prog"
  let trans_ty typ =
    print_tr "trans_ty" (print_ty typ)
  let tr_expr expr =
    print_tr "tr_expr" (print_expr expr.L.value)
  let tr_var var =
    print_tr "tr_var" (print_var var.L.value)
  let tr_simple_var sym =
    print_tr "tr_simple_var" (print_simple_var sym)
  let tr_field_var var field =
    print_tr "tr_field_var" (print_field_var var field)
  let tr_subscript_var var sub =
    print_tr "tr_subscript_var" (print_subscript_var var sub)
  let tr_call f args =
    print_tr "tr_call" (print_call f args)
  let tr_op l r op =
    print_tr "tr_op" (print_op l r op)
  let tr_record ty_name vfields =
    print_tr "tr_record" (print_record ty_name vfields)
  let tr_record_field name expr ty =
    print_tr "tr_record_field" (print_record_field name expr (Some ty))
  let tr_seq exprs =
    print_tr "tr_seq" (print_seq exprs)
  let tr_assign var expr =
    print_tr "tr_assign" (print_assign var expr)
  let tr_cond cond t f =
    print_tr "tr_cond" (print_cond cond t f)
  let tr_while cond body =
    print_tr "tr_while" (print_while cond body)
  let tr_for var lo hi body =
    print_tr "tr_for" (print_for var lo hi body)
  let tr_break br loop =
    let mark = match loop with
      | Some _ -> "inside"
      | None -> "outside"
    in
    trace @@ fun m -> m ">>> tr_break (%s): %s"
      mark (print_break br)

  let tr_let decs body =
    print_tr "tr_let" (print_let decs body)
  let tr_array typ size init =
    print_tr "tr_array" (print_array typ size init)

  let trans_decs decs =
    trace @@ fun m -> m ">>> trans_decs:\n%s" (print_decs decs)
  let trans_type_decs tys =
    trace @@ fun m -> m ">>> trans_type_decs:\n%s" (print_type_decs tys)
  let trans_fun_decs fs =
    print_tr "trans_fun" (print_fun_decs fs)
  let trans_fun_head fun_dec =
    print_tr "trans_fun_head" (print_fun_dec fun_dec)
  let trans_var_dec var =
    print_tr "trans_var_dec" (print_var_dec var)

  let ret_ty ty =
    trace @@ T.(fun m -> m "<-- %s (%s)" (to_string ty) (to_string (~! ty)))

  let assert_ty ty expr =
    trace @@ fun m -> m "!!! (ty) %s : %s" (print_expr expr.L.value) (T.to_string ty)
  let assert_comparison expr l r =
    trace @@ fun m -> m "!!! (comparsion) %s : %s (%s)"
      (print_expr l.L.value)
      (print_expr r.L.value)
      (print_expr expr.L.value)
  let assert_op l r =
    trace @@ fun m -> m "!!! (op) %s : int && %s : int"
      (print_expr l.L.value)
      (print_expr r.L.value)
  let assert_fun_body fun_dec result =
    trace @@ fun m -> m "!!! (fun body) %s : %s"
      (print_fun_dec fun_dec)
      (T.to_string result)

  let assert_init var init_ty =
    trace @@ fun m -> m "!!! (init) %s : %s"
      (print_var_dec var)
      (T.to_string init_ty)
end
