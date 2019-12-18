open Error
open Printf

module T = Type
module L = Location
module U = Unique
module S = Symbol
module Table = S.Table

type tenv = Type.t Table.t
type venv = Env.entry Table.t

type expr_ty = {
  expr : Translate.t;
  ty : Type.t;
}

let mk_ty ty = { expr = (); ty }

let type_mismatch_error msg l t1 t2 =
  let msg' = sprintf
      "type \"%s\" is expected, but found \"%s\""
      (T.to_string t1) (T.to_string t2) in
  type_error l @@ msg ^ msg'

let missing_field_error t name =
  id_error name @@ sprintf
    "record of type \"%s\" doesn't have field \"%s\""
    (T.to_string t) (S.name name.L.value)

let rec trans_prog expr =
  let open Env in
  ignore @@ trans_expr base_venv base_tenv (L.dummy expr)

and trans_expr venv tenv expr =
  let open Syntax in

  let rec assert_ty t expr =
    let { ty; _ } = tr_expr expr in
    if T.neq (T.actual ty) (T.actual t)
    then type_mismatch_error "" expr t ty

  and assert_int expr = assert_ty T.Int expr
  and assert_unit expr = assert_ty T.Unit expr

  and tr_expr expr =
    match expr.L.value with
    | Var var -> tr_var var
    | Nil _ -> mk_ty T.Nil
    | Int _ -> mk_ty T.Int
    | String _ -> mk_ty T.String
    | Call (f, args) -> tr_call f args
    | Op (l, op, r) -> tr_op expr l r op.L.value
    | Record (name, fields) -> tr_record name fields
    | Seq exprs -> tr_seq exprs
    | Assign (var, expr) -> tr_assign var expr
    | If (cond, t, f) -> tr_cond cond t f
    | While (cond, body) -> tr_while cond body
    | For (_, lo, hi, body, _) -> tr_for lo hi body
    | Break _ -> mk_ty T.Unit
    | Let (decs, body) -> tr_let decs body
    | Array (ty, size, init) -> tr_array ty size init

  and tr_call f args =
    match Table.find_fun f venv with
    | Env.VarEntry t ->
      type_error f @@ sprintf
        "expected function, but found variable \"%s\" of type \"%s\""
        (S.name f.L.value) (T.to_string t)
    | Env.FunEntry (formals, result) ->
      (* check if all the arguments are supplied *)
      if List.length formals <> List.length args then
        type_error f @@ sprintf
          "function \"%s\" expects %d formal arguments, but %d was given"
          (S.name f.L.value) (List.length formals) (List.length args) ;
      List.iter2 assert_ty formals args;
      mk_ty @@ T.actual result

  (* in our language binary operators work only with
     integer operands, except for (=) and (<>) *)
  and tr_op expr l r = function
    | Syntax.Eq | Syntax.Neq ->
      assert_comparison expr l r
    | _ ->
      assert_op l r

  and assert_comparison expr l r =
    let { ty = ty_l; _ } = tr_expr l in
    let { ty = ty_r; _ } = tr_expr r in
    if T.neq (T.actual ty_l) (T.actual ty_r)
    then type_mismatch_error "" expr ty_l ty_r;
    mk_ty ty_l

  and assert_op l r =
    assert_int l;
    assert_int r;
    mk_ty T.Int

  and tr_assign var expr =
    let { ty = var_ty; _ } = tr_var var in
    let { ty = expr_ty; _ } = tr_expr expr in
    if T.eq var_ty expr_ty
    then mk_ty var_ty
    else type_error expr @@ sprintf
        "invalid assigment of type \"%s\" to a variable of type \"%s\""
        (T.to_string expr_ty) (T.to_string var_ty)

  (* type of a sequence is a type of its last expression,
     but we need to check all previous expressions too *)
  and tr_seq exprs =
    List.fold_left
      (fun _ expr -> tr_expr expr)
      (mk_ty T.Unit)
      exprs

  and tr_cond cond t f =
    assert_int cond;
    let { ty = t_ty; _ } = tr_expr t in
    match f with
    | None ->
      mk_ty t_ty
    | Some f ->
      (* If there is a false-branch then we should
         check if types of both branches match *)
      let { ty = f_ty; _ } = tr_expr f in
      if T.eq t_ty f_ty
      then mk_ty t_ty
      else type_error expr @@ sprintf
          "different types of branch expressions: \"%s\" and \"%s\""
          (T.to_string t_ty) (T.to_string f_ty)

  and tr_while cond body =
    assert_int cond;
    assert_unit body;
    mk_ty T.Unit

  and tr_for lo hi body =
    assert_int lo;
    assert_int hi;
    assert_unit body;
    mk_ty T.Unit

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
    match T.actual rec_typ with
    | T.Record (tfields, _) ->
      (* we want to check each field of the variable
         against the corresponding record type definition *)
      List.iter
        (fun (name, expr) ->
           (* find a type of the field with [name] *)
           match List.assoc_opt name.L.value tfields with
           | Some tvn ->
             let { ty = tn; _ } = tr_expr expr in
             let ty = T.actual tn in
             let tv = T.actual tvn in
             if T.neq ty tv
             then type_mismatch_error "" expr ty tv
           | None -> missing_field_error rec_typ name
        )
        vfields;
      mk_ty rec_typ
    | _ ->
      type_error ty_name @@ sprintf
        "\"%s\" is not a record" (T.to_string rec_typ)

  and tr_array typ size init =
    assert_int size;
    let { ty = init_tn; _ } = tr_expr init in
    (* find the type of this particular array *)
    let arr_ty = Table.find_ty typ tenv in
    match T.actual arr_ty with
    | T.Array (tn, _) ->
      let t = T.actual tn in
      let init_t = T.actual init_tn in
      if T.eq t init_t
      then mk_ty arr_ty
      else type_mismatch_error
          "invalid type of array initial value, " init t init_t
    | _ ->
      type_error typ @@ sprintf
        "\"%s\" is not array" (T.to_string arr_ty)

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
    | Env.VarEntry tn ->
      mk_ty @@ T.actual tn
    | Env.FunEntry (formals, result) ->
      let signature =
        formals
        |> List.map T.to_string
        |> String.concat ", " in
      type_error var @@ sprintf
        "expected variable, but found a function \"(%s) : %s\""
        signature (T.to_string result)

  and tr_field_var var field =
    (* find a type of the record variable *)
    let rec_ty = tr_var var in
    (* lets see if its actually a record *)
    match rec_ty.ty with
    | Record (fields, _) ->
      (try
         (* record is just a list of pairs (S.t * Type.t),
            lets try to find a type for the given [field],
            it is the type of the FieldVar expression  *)
         mk_ty @@ T.actual @@ List.assoc field.L.value fields
       with Not_found ->
         missing_field_error rec_ty.ty field)
    | _ ->
      type_error var @@ sprintf
        "expected record, but \"%s\" found"
        (T.to_string rec_ty.ty)

  and tr_subscript_var var sub =
    let arr_ty = tr_var var in
    match arr_ty.ty with
    | Array (tn, _) ->
      assert_int sub;
      mk_ty @@ T.actual tn
    | _ ->
      type_error var @@ sprintf
        "\"%s\" is not an array" (T.to_string arr_ty.ty)

  in tr_expr expr

and trans_decs venv tenv =
  List.fold_left
    (fun (venv, tenv) dec -> trans_dec venv tenv dec)
    (venv, tenv)

(* modifies and returns term-level and
   type-level environments adding the given declaration *)
and trans_dec venv tenv = function
  | TypeDec tys -> trans_tys venv tenv tys
  | FunDec fs -> trans_funs venv tenv fs
  | VarDec var -> trans_var venv tenv var

and trans_tys venv tenv tys =
  let open Syntax in
  let tr_ty_head (tns, tenv) ty_dec =
    let sym = ty_dec.L.value.type_name.L.value in
    let tn = ref None in
    (* add a type-reference, without body *)
    let tenv' = Table.add sym (T.Name (sym, tn)) tenv in
    tn :: tns, tenv' in
  (* first, we add all the type names to the [tenv] *)
  let (tns, tenv') = List.fold_left tr_ty_head ([], tenv) tys in
  let resolve_ty tn ty_dec =
    let { typ; _ } = ty_dec.L.value in
    (* resolve the type body *)
    let t = trans_ty tenv' typ in
    tn := Some t in
  (* resolve the (possibly mutually recursive) types *)
  List.iter2 resolve_ty (List.rev tns) tys;
  venv, tenv'

and trans_funs venv tenv fs =
  let open Syntax in
  let open Env in
  let tr_fun_head (sigs, venv) fun_dec =
    let { fun_name; params; result_typ; _ } = fun_dec.L.value in
    (* translate function arguments *)
    let tr_arg f = f.name.L.value, Table.find_ty f.typ tenv in
    let args = List.map tr_arg params in
    let formals = List.map snd args in
    (* translate the result type *)
    let result = match result_typ with
      | None -> T.Unit
      | Some t -> Table.find_ty t tenv in
    let name = fun_name.L.value in
    let entry = FunEntry (formals, result) in
    let venv' = Table.add name entry venv in
    ((args, result) :: sigs, venv') in
  (* first, we add all the function entries to the [venv] *)
  let (sigs, venv') = List.fold_left tr_fun_head ([], venv) fs in
  let assert_fun_body (args, result) fun_dec =
    let { body; _ } = fun_dec.L.value in
    (* now, lets build another [venv''] to be used for body processing
       it should have all the arguments in it *)
    let add_var e (name, t) = Table.add name (VarEntry t) e in
    let venv'' = List.fold_left add_var venv' args in
    let { ty = body_ty; _ } = trans_expr venv'' tenv body in
    if T.neq body_ty result
    then type_mismatch_error
        "type of the body expression doesn't match the declared result type, "
        body result body_ty; in
  (* now, lets check the bodies *)
  List.iter2 assert_fun_body (List.rev sigs) fs;
  venv', tenv

and trans_var venv tenv var =
  let open Syntax in
  let { var_name; var_typ; init; _ } = var.L.value in
  let { ty = init_ty; _ } = trans_expr venv tenv init in

  (* lets see if the variable is annotated *)
  (match var_typ with
   | None -> ()
   | Some ann_ty ->
     (* check if the init expression has the
        same type as the variable annotation *)
     let var_ty = T.actual @@ Table.find_ty ann_ty tenv in
     if T.neq var_ty (T.actual init_ty)
     then type_mismatch_error "" init var_ty init_ty
  );
  let entry = Env.VarEntry init_ty in
  let venv' = Table.add var_name.L.value entry venv in
  venv', tenv

(* translates an AST type expression into a
   digested type description that we keep in the [tenv] *)
and trans_ty tenv typ =
  let open Syntax in
  match typ with
  | NameTy t ->
    Table.find_ty t tenv
  | RecordTy dec_fields ->
    let to_field { name; typ; _ } =
      name.L.value, Table.find_ty typ tenv in
    let ty_fields = List.map to_field dec_fields in
    T.Record (ty_fields, U.mk ())
  | ArrayTy t ->
    T.Array (Table.find_ty t tenv, U.mk ())
