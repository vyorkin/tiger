open Core_kernel
open Ir

let rec indent s = function
  | 0 -> s
  | i -> "  " ^ indent s (i - 1)

and p_expr i = function
  | Const x ->
    indent (sprintf "CONST %d" x) i
  | Name l ->
    indent (sprintf "NAME (%s)" (Temp.print_label l)) i
  | Temp t ->
    indent (sprintf "TEMP (%s)" (Temp.print_temp t)) i
  | BinOp (l, binop, r) ->
    indent (
      sprintf
        "BINOP\n%s\n%s\n%s"
        (indent (String.uppercase @@ Ir.show_binop binop) (i + 1))
        (p_expr (i + 2) l)
        (p_expr (i + 2) r)
    ) i
  | Mem expr ->
    indent (sprintf "MEM\n%s" (p_expr (i + 1) expr)) i
  | Call (f, args) ->
    let args_s =
      args
      |> List.map ~f:(p_expr (i + 1))
      |> String.concat ~sep:"\n"
    in
    indent (sprintf "CALL\n%s\n%s" (p_expr (i + 1) f) args_s) i
  | ESeq (stmt, expr) ->
    indent (
      sprintf "ESEQ\n%s\n%s"
        (p_stmt (i + 1) stmt)
        (p_expr (i + 1) expr)
    ) i

and p_stmt i = function
  | Move (dst, src) ->
    indent (
      sprintf "MOVE\n%s\n%s"
        (p_expr (i + 1) dst)
        (p_expr (i + 1) src)
    ) i
  | Expr expr ->
    indent (sprintf "EXPR\n%s" (p_expr (i + 1) expr)) i
  | Jump (expr, _) ->
    indent (sprintf "JUMP\n%s" (p_expr (i + 1) expr)) i
  | CJump { op; left; right; t; f } ->
    indent (
      sprintf "CJUMP\n%s\n%s\n%s\n%s\n%s"
        (indent (String.uppercase @@ Ir.show_relop op) (i + 1))
        (p_expr (i + 2) left)
        (p_expr (i + 2) right)
        (indent ("T: " ^ Temp.print_label t) (i + 1))
        (indent ("F: " ^ Temp.print_label f) (i + 1))
    ) i
  | Seq (e1, e2) ->
    indent (
      sprintf "SEQ\n%s\n%s"
        (p_stmt (i + 1) e1)
        (p_stmt (i + 1) e2)) i
  | Label temp ->
    indent (sprintf "LABEL (%s)" (Temp.print_label temp)) i


let print_expr expr = p_expr 0 expr
and print_stmt stmt = p_stmt 0 stmt
