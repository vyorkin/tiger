open Core_kernel

module L = Location
module S = Symbol

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
} [@@deriving show]

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
  let print_ty = function
    | NameTy _ -> "name"
    | RecordTy _ -> "record"
    | ArrayTy _ -> "array"

  let rec print_expr = function
    | Var var ->
      print_var var.L.value
    | Nil _ ->
      "()"
    | Int x ->
      sprintf "%s : int" (Int.to_string x.L.value)
    | String s ->
      sprintf "\"%s\" : string" s.L.value
    | Call (f, args) ->
      print_call f args
    | Op (l, op, r) ->
      print_op l r op.L.value
    | Record _ ->
      "[record]"
    | Seq _ ->
      "[seq]"
    | Assign _ ->
      "[assign]"
    | If _ ->
      "[if]"
    | While _ -> "[while]"
    | For _ -> "[for]"
    | Break _ -> "[break]"
    | Let _ -> "[let]"
    | Array _ -> "[array]"

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

  and print_dec _ = ""

  and print_var_dec _ = ""

  and print_type_dec _ = ""

  and print_fun_dec _ = ""

  and print_symbol sym =
    sprintf "%s <#%d>" sym.L.value.S.name sym.L.value.id

end
