open Core_kernel

module S = Symbol
module L = Location
module T = Type
module Tr = Translate

type target =
  | Stdout
  | File of string
[@@deriving eq, show]

type source =
  | Symbol of target list
  | Semant of target list
[@@deriving eq, show]

(* Note that in the [logs] lib terminology "source" defines a
   named unit of logging whose reporting level can be set independently *)

module SymbolTable = struct
  let src = Logs.Src.create "tig.symbol-table" ~doc:"Symbol table"

  let trace_loc op name sym =
    Logs.debug ~src (fun m ->
        m ~header:"symbol" "%s %s: %s" op name (S.to_string_loc sym))

  let bind name sym = trace_loc "<==" name sym
  let look name sym = trace_loc "==>" name sym

  let trace_label op sym =
    Logs.debug ~src (fun m ->
        m ~header:"symbol" "%s: %s" op (S.to_string sym))

  let add_label name  = trace_label "<|-" name
  let find_label name = trace_label "-|>" name
end

module SemanticAnalysis = struct
  open Syntax
  open Syntax_printer

  let src = Logs.Src.create "tig.semantic-analysis" ~doc:"Semantic analysis"
  let trace f = Logs.debug ~src (fun m -> f (m ~header:"semant"))
  let trace_tr name expr = trace @@ fun m -> m ">>> %s: %s" name expr

  let trans_prog _ =
    trace (fun m -> m "trans_prog")
  let trans_ty typ =
    trace_tr "trans_ty" (print_ty typ)
  let tr_expr expr =
    trace_tr "tr_expr" (print_expr expr.L.value)
  let tr_var var =
    trace_tr "tr_var" (print_var var.L.value)
  let tr_simple_var sym =
    trace_tr "tr_simple_var" (print_simple_var sym)
  let tr_field_var var field =
    trace_tr "tr_field_var" (print_field_var var field)
  let tr_subscript_var var sub =
    trace_tr "tr_subscript_var" (print_subscript_var var sub)
  let tr_call f args =
    trace_tr "tr_call" (print_call f args)
  let tr_op l op r =
    trace_tr "tr_op" (print_op l op r)
  let tr_record ty_name vfields =
    trace_tr "tr_record" (print_record ty_name vfields)
  let tr_record_field name expr ty =
    trace_tr "tr_record_field" (print_record_field name expr (Some ty))
  let tr_seq exprs =
    trace_tr "tr_seq" (print_seq exprs)
  let tr_assign var expr =
    trace_tr "tr_assign" (print_assign var expr)
  let tr_cond cond t f =
    trace_tr "tr_cond" (print_cond cond t f)
  let tr_then () =
    trace (fun m -> m "tr_then")
  let tr_else () =
    trace (fun m -> m "tr_else")
  let tr_while cond body =
    trace_tr "tr_while" (print_while cond body)
  let tr_for var lo hi body escapes =
    trace_tr "tr_for" (print_for var lo hi body escapes)
  let tr_break br done_l =
    let mark = match done_l with
      | Some l -> "inside: " ^ Temp.print_label l
      | None -> "outside"
    in
    trace @@ fun m -> m ">>> tr_break (%s): %s"
      mark (print_break br)

  let tr_let decs body =
    trace_tr "tr_let" (print_let decs body)
  let tr_array typ size init =
    trace_tr "tr_array" (print_array typ size init)

  let trans_decs decs =
    trace @@ fun m -> m ">>> trans_decs:\n%s" (print_decs decs)
  let trans_type_decs tys =
    trace @@ fun m -> m ">>> trans_type_decs:\n%s" (print_type_decs tys)
  let trans_fun_decs fs =
    trace @@ fun m -> m ">>> trans_fun:\n%s" (print_fun_decs fs)
  let trans_fun_head fun_dec =
    trace_tr "trans_fun_head" (print_fun_dec fun_dec)
  let trans_var_dec var =
    trace_tr "trans_var_dec" (print_var_dec var)

  let ret expr ty =
    let open T in
    trace @@ fun m -> m "<-- %s (%s):\n%s"
      (to_string ty)
      (to_string (~! ty))
      (Tr.Printer.print_expr expr)

  let assert_ty t1 t2 =
    trace @@ fun m -> m "!!! (ty) %s = %s" (T.to_string t1) (T.to_string t2)

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

module SyntaxRewriting = struct
  open Syntax
  open Syntax_printer

  let src = Logs.Src.create "tig.syntax-rewriting" ~doc:"Syntax rewriting"
  let trace f = Logs.debug ~src (fun m -> f (m ~header:"rewrite"))

  let rewrite_for var lo hi body escapes =
    trace @@ fun m -> m " %s" (print_for var lo hi body escapes)
end

module StackFrame = struct
  open Frame.Printer

  let src = Logs.Src.create "tig.stack-frame" ~doc:"Stack frames"
  let trace f = Logs.debug ~src (fun m -> f (m ~header:"frame"))

  let mk frame =
    trace @@ fun m -> m " mk: \n%s" (print_frame frame)
end

module Translation = struct
  open Tr
  open Frame.Printer

  let src = Logs.Src.create "tig.translation" ~doc:"Translation"
  let trace f = Logs.debug ~src (fun m -> f (m ~header:"translate"))

  let new_level level =
    let path =
      level
      |> frames_path
      |> List.map ~f:Int.to_string
      |> String.concat ~sep:"->"
    in
    StackFrame.mk level.frame;
    trace @@ fun m -> m " new_level: %s" path

  let alloc_local access =
    let (lev, acc) = access in
    trace @@ fun m -> m " alloc_local #%d: %s" (Frame.id lev.frame) (print_access acc)
end

module Escaping = struct
  open Syntax
  open Syntax_printer

  let src = Logs.Src.create "tig.escaping" ~doc:"Calculating escapes"
  let trace f = Logs.debug ~src (fun m -> f (m ~header:"escape"))

  let escapes sym depth =
    trace @@ fun m -> m " %s escapes at depth %d"
      (print_symbol sym) depth
end

(* TODO: Report only enabled sources *)
let mk_reporter _cfg =
  let open Config in
  let app = Format.std_formatter in
  let dst = Format.err_formatter in
  let report _src (level : Logs.level) ~over k msgf =
    let k _ = over (); k () in
    msgf @@ fun ?header ?tags:__ fmt ->
    (* [ppf] is our pretty-printing formatter
       see https://ocaml.org/learn/tutorials/format.html#Most-general-pretty-printing-using-fprintf for deatils *)
    let ppf = match level with
      | Logs.App -> app
      | _ -> dst
    in
    Format.kfprintf k ppf ("%a@[" ^^ fmt ^^ "@]@.") Logs_fmt.pp_header (level, header)
  in
  { Logs.report = report }
