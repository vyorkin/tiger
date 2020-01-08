open Core_kernel
open Syntax

module L = Location
module S = Symbol
module T = Type

let rec print_expr = function
  | Var var ->
    print_var var.L.value
  | Nil _ ->
    "()"
  | Int x ->
    print_int x
  | String s ->
    print_string s
  | Call (f, args) ->
    print_call f args
  | Op (l, op, r) ->
    print_op l r op.L.value
  | Record (ty_name, vfields) ->
    print_record ty_name vfields
  | Seq exprs ->
    print_seq exprs
  | Assign (var, expr) ->
    print_assign var expr
  | If (cond, t, f) ->
    print_cond cond t f
  | While (cond, body) ->
    print_while cond body
  | For (var, lo, hi, body, escapes) ->
    print_for var lo hi body !escapes
  | Break br ->
    print_break br
  | Let (decs, body) ->
    print_let decs body
  | Array (typ, size, init) ->
    print_array typ size init

and print_ty = function
  | NameTy sym ->
    print_symbol sym
  | RecordTy fields ->
    sprintf "\n{\n%s\n}" (print_fields fields ~sep:";\n")
  | ArrayTy sym ->
    sprintf "[%s]" (print_symbol sym)

and print_fields fields ~sep =
  fields
  |> List.map ~f:print_field
  |> String.concat ~sep

and print_field field =
  sprintf "%s%s : %s"
    (print_symbol field.name)
    (print_esc field.escapes)
    (print_symbol field.typ)

and print_int x =
  Int.to_string x.L.value

and print_string s =
  sprintf "\"%s\"" s.L.value

and print_op l r op =
  sprintf "%s %s %s"
    (print_expr l.L.value)
    (print_op_sym op)
    (print_expr r.L.value)

and print_op_sym = function
  | Plus   -> "+"
  | Minus  -> "-"
  | Times  -> "*"
  | Divide -> "/"
  | Ge     -> ">="
  | Gt     -> ">"
  | Le     -> "<="
  | Lt     -> "<"
  | Eq     -> "="
  | Neq    -> "<>"

and print_call f args =
  sprintf "%s(%s)"
    (print_symbol f)
    (print_call_args args)

and print_call_args args =
  args
  |> List.map ~f:(fun arg -> print_expr arg.L.value)
  |> String.concat ~sep:", "

and print_var = function
  | SimpleVar var ->
    print_simple_var var
  | FieldVar (var, field) ->
    print_field_var var field
  | SubscriptVar (var, sub) ->
    print_subscript_var var sub

and print_simple_var var =
  print_symbol var

and print_field_var var field =
  sprintf "%s.%s"
    (print_var var.L.value)
    (print_symbol field)

and print_subscript_var var sub =
  sprintf "%s[%s]"
    (print_var var.L.value)
    (print_expr sub.L.value)

and print_decs decs =
  decs
  |> List.map ~f:print_dec
  |> String.concat ~sep:"\n"

and print_dec = function
  | TypeDec tys ->
    print_type_decs tys
  | FunDec fs ->
    print_fun_decs fs
  | VarDec var ->
    print_var_dec var

and print_type_decs tys =
  tys
  |> List.map ~f:print_type_dec
  |> String.concat ~sep:"\n"

and print_fun_decs fs =
  fs
  |> List.map ~f:print_fun_dec
  |> String.concat ~sep:"\n"

and print_type_dec ty_dec =
  let ty = ty_dec.L.value in
  sprintf "%s = %s"
    (print_symbol ty.type_name)
    (print_ty ty.typ)

and print_fun_dec fun_dec =
  let f = fun_dec.L.value in
  sprintf "function %s(%s)%s = ..."
    (print_symbol f.fun_name)
    (print_fields f.params ~sep:", ")
    (Option.value_map f.result_typ ~default:""
       ~f:(fun s -> sprintf " : %s" @@ print_symbol s))

and print_var_dec var =
  let v = var.L.value in
  sprintf "%s%s%s := %s"
    (print_symbol v.var_name)
    (Option.value_map v.var_typ ~default:"" ~f:print_symbol)
    (print_esc var.L.value.escapes)
    (print_expr v.init.L.value)

and print_seq exprs =
  sprintf "(;%d;)" (List.length exprs)

and print_assign var expr =
  sprintf "%s := %s"
    (print_var var.L.value)
    (print_expr expr.L.value)

and print_record ty_name vfields =
  sprintf "\n%s :=\n{\n  %s\n}"
    (print_symbol ty_name)
    (print_record_fields vfields)

and print_record_fields fields =
  fields
  |> List.map ~f:(fun (name, expr) -> print_record_field name expr None)
  |> String.concat ~sep:",\n"

and print_record_field name expr ty =
  sprintf "\n  %s%s = %s"
    (print_symbol name)
    (print_expr expr.L.value)
    (Option.value_map ty ~default:"" ~f:T.to_string)

and print_cond cond t f =
  sprintf "if %s then %s%s"
    (print_expr cond.L.value)
    (print_expr t.L.value)
    (Option.value_map f ~default:""
       ~f:(fun e -> sprintf " else %s" @@ print_expr e.L.value))

and print_while cond body =
  sprintf "\nwhile %s\n  %s"
    (print_expr cond.L.value)
    (print_expr body.L.value)

and print_for var lo hi body escapes =
  sprintf "\nfor %s%s := %s to %s do %s"
    (print_symbol var)
    (print_esc escapes)
    (print_expr lo.L.value)
    (print_expr hi.L.value)
    (print_expr body.L.value)

and print_break _ =
  "break"

and print_let decs body =
  sprintf "\nlet\n%s\nin\n%s"
    (print_decs decs)
    (print_expr body.L.value)

and print_array typ size init =
  sprintf "%s[%s] of %s"
    (print_symbol typ)
    (print_expr size.L.value)
    (print_expr init.L.value)

and print_symbol sym =
  let s = sym.L.value in
  sprintf "%s <#%d>" s.name s.id

and print_esc esc =
  if esc then " (+)" else " (-)"
