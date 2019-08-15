open Error
open Printf

module T = Type
module L = Location
module S = Symbol
module Table = S.Table

type tenv = Type.t Table.t
type venv = Env.entry Table.t

type expr_ty = {
  expr : Translate.t;
  ty : Type.t;
}

let ty t = {
  expr = ();
  ty = T.actual t
}

let type_mismatch_error msg l t1 t2 =
  let msg' = sprintf
      "type %s is expected, but found %s"
      (T.show t1) (T.show t2) in
  type_error l @@ msg ^ msg'

let missing_field_error t name =
  id_error name @@ sprintf
    "record of type %s doesn't have field %s"
    (T.show t) (S.name name.L.value)

let rec trans_prog expr =
  let open Env in
  ignore (trans_expr base_venv base_tenv (L.dummy expr))

and trans_expr venv tenv expr =
  let open Syntax in

  let rec assert_ty t expr =
    let { ty; _ } = tr_expr expr in
    if ty <> t then type_mismatch_error "" expr t ty

  and assert_int expr  = assert_ty T.Int expr
  and assert_unit expr = assert_ty T.Unit expr

  and tr_expr expr =
    match expr.L.value with
    | Var var -> tr_var var
    | Nil _ -> ty T.Nil
    | Int _ -> ty T.Int
    | String _ -> ty T.String
    | Call (f, args) -> tr_call f args
    | Op (l, _, r) -> tr_op l r
    | Record (name, fields) -> tr_record name fields
    | Seq exprs -> tr_seq exprs
    | Assign (var, expr) -> tr_assign var expr
    | If (cond, t, f) -> tr_cond cond t f
    | While (cond, body) -> tr_while cond body
    | For (_, lo, hi, body, _) -> tr_for lo hi body
    | Break _ -> ty T.Unit
    | Let (decs, body) -> tr_let decs body
    | Array (ty, size, init) -> tr_array ty size init

  and tr_call f args =
    match Table.find_fun f venv with
    | Env.VarEntry t ->
      type_error f @@ sprintf
        "expected function, but found variable %s of type %s"
        (S.name f.L.value) (T.show t)
    | Env.FunEntry (formals, result) ->
      List.iter2 assert_ty formals args;
      ty result

  (* in our language binary operators
   * work only with integer operands *)
  and tr_op l r =
    assert_int l;
    assert_int r;
    ty T.Int

  and tr_assign var expr =
    let { ty = var_ty; _ } = tr_var var in
    let { ty = expr_ty; _ } = tr_expr expr in
    if var_ty = expr_ty
    then ty var_ty
    else type_error expr @@ sprintf
        "invalid assigment of type %s to a variable of type %s"
        (T.show expr_ty) (T.show var_ty)

  (* Type of a sequence is a type of its last expression,
   * but we need to check all previous expressions too *)
  and tr_seq exprs =
    List.fold_left
      (fun _ expr -> tr_expr expr)
      (ty T.Unit)
      exprs

  and tr_cond cond t f =
    assert_int cond;
    let { ty = t_ty; _ } = tr_expr t in
    match f with
    | None ->
      ty t_ty
    | Some f ->
      (* If there is a false-branch then we should
       * check if types of both branches match *)
      let { ty = f_ty; _ } = tr_expr f in
      if t_ty = f_ty
      then ty t_ty
      else type_error expr @@ sprintf
        "different types of branch expressions: %s and %s"
        (T.show t_ty) (T.show f_ty)

  and tr_while cond body =
    assert_int cond;
    assert_unit body;
    ty T.Unit

  and tr_for lo hi body =
    assert_int lo;
    assert_int hi;
    assert_unit body;
    ty T.Unit

  (* in Tiger declarations appear only in a "let" expression,
     the let expression modifies both:
     type-level (tenv) and term-level (venv) environments *)
  and tr_let decs body =
    (* update env's according to declarations *)
    let venv', tenv' = trans_decs venv tenv decs in
    (* then translate the body expression using
       the new augmented environments *)
    trans_expr venv' tenv' body
    (* then the new environments are discarded *)

  and tr_record ty_name vfields =
    (* get the record type definition *)
    let rec_typ = Table.find_ty ty_name tenv in
    match rec_typ with
    | T.Record (tfields, _) ->
      (* we want to check each field of the record variable
       * against its type definition (every field) *)
      List.iter
        (fun (name, expr) ->
           (* find a type of the field with [name] *)
           match List.assoc_opt name.L.value tfields with
           | Some t -> assert_ty t expr;
           | None -> missing_field_error rec_typ name
        )
        vfields;
      ty rec_typ
    | _ ->
      type_error ty_name @@ sprintf
      "%s is not a record" (T.show rec_typ)

  and tr_array typ size init =
    assert_int size;
    let { ty = init_ty; _ } = tr_expr init in
    (* find the type of this particular array *)
    let arr_ty = Table.find_ty typ tenv in
    match arr_ty with
    | T.Array (t, _) ->
      if init_ty = t
      then ty arr_ty
      else type_mismatch_error
          "invalid type of array initial value, " init t init_ty
    | _ ->
      type_error typ @@ sprintf
      "%s is not array" (T.show arr_ty)

  and tr_var var =
    match var.L.value with
    | SimpleVar var ->
      tr_simple_var var
    | FieldVar (var, field) ->
      tr_field_var var field
    | SubscriptVar (var, sub) ->
      tr_subscript_var var sub

  and tr_simple_var var =
    match Table.find_var var venv with
    | Env.VarEntry t ->
      ty t
    | Env.FunEntry (formals, result) ->
      let signature =
        formals
        |> List.map T.show
        |> String.concat ", " in
      type_error var @@ sprintf
        "expected variable, but found a function (%s) : %s"
        signature (T.show result)

  and tr_field_var var field =
    (* find a type of the record variable *)
    let rec_ty = tr_var var in
    (* lets see if its actually a record *)
    match rec_ty.ty with
    | Record (fields, _) ->
      (try
        (* record is just a list of pairs (S.t * Type.t),
         * lets try to find a type for the given [field],
         * it is the type of the FieldVar expression  *)
         ty @@ List.assoc field.L.value fields
       with Not_found ->
         missing_field_error rec_ty.ty field)
    | _ ->
      type_error var @@ sprintf
        "expected record, but %s found"
        (T.show rec_ty.ty)

  and tr_subscript_var var sub =
    let arr_ty = tr_var var in
    match arr_ty.ty with
    | Array (t, _) ->
      assert_int sub;
      ty t
    | _ ->
      type_error var @@ sprintf
        "%s is not an array" (T.show arr_ty.ty)

  in tr_expr expr

and trans_decs venv tenv =
  List.fold_left
    (fun (venv, tenv) dec -> trans_dec venv tenv dec)
    (venv, tenv)

(* modifies and returns term-level and
 * type-level environments adding the given declaration *)
and trans_dec venv tenv dec =
  let open Syntax in
  match dec with
  | TypeDec type_dec ->
    let { type_name; typ } = type_dec.L.value in
    let t = trans_ty tenv typ in
    let tenv' = Table.add type_name.L.value t tenv in
    venv, tenv'
  | VarDec var ->
    trans_var venv tenv var
  | FunDec f ->
    trans_fun venv tenv f

and trans_var venv tenv var =
  let open Syntax in
  let { var_name; var_typ; init; _ } = var.L.value in
  let { ty = init_ty; _ } = trans_expr venv tenv init in
  (* lets see if the variable is annotated *)
  (match var_typ with
  | None -> ()
  | Some ann_ty ->
    (* check if the init expression has the
     * same type as the variable annotation *)
    let var_ty = Table.find_ty ann_ty tenv in
    if var_ty <> init_ty
      then type_mismatch_error "" init var_ty init_ty
  );
  let entry = Env.VarEntry init_ty in
  let venv' = Table.add var_name.L.value entry venv in
  (venv', tenv)

and trans_fun venv tenv fn =
  let open Syntax in
  let open Env in
  let { fun_name; params; body; result_typ; } = fn.L.value in
  let tr_arg f = f.name.L.value, Table.find_ty f.typ tenv in
  let args = List.map tr_arg params in
  let result = match result_typ with
    | None -> T.Unit
    | Some t -> Table.find_ty t tenv in
  let add_fun name env =
    let formals = List.map snd args in
    let entry = FunEntry (formals, result) in
    Table.add name entry env in
  let add_var env (name, t) = Table.add name (VarEntry t) env in
  (* resulting environment that is used for
   * processing expressions that allowed to call [fn] *)
  let venv' = add_fun fun_name.L.value venv in
  (* this environment is used to process the body *)
  let venv'' = List.fold_left add_var venv' args in
  let { ty = body_ty; _ } = trans_expr venv'' tenv body in
  let () =
    if body_ty <> result
    then type_mismatch_error
        "type of the body expression doesn't match the declared result type, "
        body result body_ty in
  venv', tenv

(* translates a AST type expression into
 * a digested type description that we keed in the type-level environment *)
and trans_ty tenv typ =
  let open Syntax in
  match typ with
  | NameTy t ->
    Table.find_ty t tenv
  | RecordTy dec_fields ->
    let to_field { name; typ; _ } =
      name.L.value, Table.find_ty typ tenv in
    let ty_fields = List.map to_field dec_fields in
    T.Record (ty_fields, ref ())
  | ArrayTy t ->
    T.Array (Table.find_ty t tenv, ref ())
