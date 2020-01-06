open Core_kernel

module L = Location
module S = Symbol
module T = Type

type op =
  (* arithmetics *)
  | Plus
  | Minus
  | Times
  | Divide
  (* comparison *)
  | Ge
  | Gt
  | Le
  | Lt
  | Eq
  | Neq
[@@deriving show { with_path = false }]

type field = {
  name : S.t L.t;
  typ : S.t L.t;
  escape : bool ref;
} [@@deriving show { with_path = false }]

(* Type *)
type ty =
  | NameTy of S.t L.t
  | RecordTy of field list
  | ArrayTy of S.t L.t
[@@deriving show { with_path = false }]

type expr =
  | Var of var L.t
  | Nil of unit L.t
  | Int of int L.t
  | String of string L.t
  | Call of S.t L.t * (* name *)
            expr L.t list (* args *)
  | Op of expr L.t * (* left operand *)
          op L.t * (* operator *)
          expr L.t (* right operand *)
  | Record of S.t L.t * (* record type name *)
              (S.t L.t * expr L.t) list (* fields *)
  | Seq of expr L.t list
  | Assign of var L.t *
              expr L.t
  | If of expr L.t * (* condition *)
          expr L.t * (* then *)
          expr L.t option (* else *)
  | While of expr L.t * (* condition *)
             expr L.t (* body *)
  | For of S.t L.t * (* iterator name *)
           expr L.t * (* from *)
           expr L.t * (* to *)
           expr L.t * (* body *)
           bool ref (* escape *)
  | Break of unit L.t
  | Let of dec list * (* declarations *)
           expr L.t (* body *)
  | Array of S.t L.t * (* type *)
             expr L.t * (* size *)
             expr L.t (* init *)
[@@deriving show { with_path = false }]

(* Variable *)
and var =
  | SimpleVar of S.t L.t
  | FieldVar of
      var L.t * (* var *)
      S.t L.t (* field *)
  | SubscriptVar of
      var L.t * (* var *)
      expr L.t (* subscript / index *)
[@@deriving show { with_path = false }]

(* Type(s), value or function(s) declaration *)
and dec =
  | TypeDec of type_dec L.t list
  | FunDec of fun_dec L.t list
  | VarDec of var_dec L.t
[@@deriving show { with_path = false }]

(* Value declaration *)
and var_dec = {
  var_name : S.t L.t;
  var_typ : S.t L.t option;
  init : expr L.t;
  escape : bool ref;
} [@@deriving show { with_path = false }]

(* Type declaration *)
and type_dec = {
  type_name : S.t L.t;
  typ : ty;
} [@@deriving show { with_path = false }]

(* Function declaration *)
and fun_dec = {
  fun_name : S.t L.t;
  params : field list;
  body : expr L.t;
  result_typ : S.t L.t option;
} [@@deriving show { with_path = false }]

module Printer = struct
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
    | For (var, lo, hi, body, _) ->
      print_for var lo hi body
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
    sprintf "%s : %s" (print_symbol field.name) (print_symbol field.typ)

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
    sprintf "%s%s := %s"
      (print_symbol v.var_name)
      (Option.value_map v.var_typ ~default:"" ~f:print_symbol)
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

  and print_for var lo hi body =
    sprintf "\nfor %s := %s to %s do\n  %s"
      (print_symbol var)
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
end
