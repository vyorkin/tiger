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
  [@@deriving show]

type field = {
  name : S.t L.t;
  typ : S.t L.t;
  escape : bool ref;
} [@@deriving show]

(* Type *)
type ty =
  | NameTy of S.t L.t
  | RecordTy of field list
  | ArrayTy of S.t L.t
  [@@deriving show]

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
  | Record of S.t L.t * (* name *)
              (S.t L.t * expr L.t) list (* fields *)
  | Seq of expr L.t list
  | Assign of var L.t *
              expr L.t
  | If of expr L.t * (* condition *)
          expr L.t * (* then *)
          expr L.t option (* else *)
  | While of expr L.t * (* condition *)
             expr L.t (* body *)
  | For of S.t * (* iterator name *)
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
  [@@deriving show]

(* Variable *)
and var =
  | SimpleVar of S.t L.t
  | FieldVar of var L.t * S.t L.t
  | SubscriptVar of var L.t * expr L.t
  [@@deriving show]

(* Type, value or function declaration *)
and dec =
  | TypeDec of type_dec L.t
  | VarDec of var_dec L.t
  | FunDec of fun_dec L.t
  [@@deriving show]

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
} [@@deriving show]

(* Function declaration *)
and fun_dec = {
  fun_name : S.t L.t;
  params : field list;
  body : expr L.t;
  result_typ : S.t L.t option;
} [@@deriving show]
