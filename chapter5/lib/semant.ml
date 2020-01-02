open Core_kernel
open Err

module T = Type
module L = Location
module U = Unique
module S = Symbol

type tenv = Type.t S.Table.t
type venv = Env.entry S.Table.t

type params =
  { trace : bool
  }

type ctx = {
  tenv : tenv;
  venv : venv;
  path : (Syntax.expr Location.t) list;
  params : params
}

type expr_ty =
  { expr : Translate.t;
    ty : Type.t;
  }

let mk_ty ty = { expr = (); ty }

let type_mismatch_error4 msg l t1 t2 =
  let msg' = sprintf
      "type \"%s\" is expected, but found \"%s\""
      (T.to_string t1) (T.to_string t2) in
  type_error l @@ msg ^ msg'

let type_mismatch_error3 l t1 t2 =
  type_mismatch_error4 "" l t1 t2

let missing_field_error t name =
  id_error name @@ sprintf
    "record of type \"%s\" doesn't have field \"%s\""
    (T.to_string t) (name.L.value.S.name)

let rec trans_prog expr ~params =
  let ctx = {
    tenv = Env.base_tenv;
    venv = Env.base_venv;
    path = [];
    params
  } in
  ignore @@ trans_expr (L.dummy expr) ~ctx

and trans_expr expr ~ctx =
  let open Syntax in

  let rec assert_ty t expr ~ctx =
    let { ty; _ } = tr_expr expr ~ctx in
    if T.(~!ty <> ~!t)
    then type_mismatch_error3 expr t ty

  and assert_int expr ~ctx = assert_ty T.Int expr ~ctx
  and assert_unit expr ~ctx = assert_ty T.Unit expr ~ctx

  and tr_expr expr ~ctx =
    (* push the current [expr] to the [path] stack *)
    let ctx = { ctx with path = expr :: ctx.path } in
    match expr.L.value with
    | Var var -> tr_var var ~ctx
    | Nil _ -> mk_ty T.Nil
    | Int _ -> mk_ty T.Int
    | String _ -> mk_ty T.String
    | Call (f, args) -> tr_call f args ~ctx
    | Op (l, op, r) -> tr_op expr l r op.L.value ~ctx
    | Record (name, fields) -> tr_record name fields ~ctx
    | Seq exprs -> tr_seq exprs ~ctx
    | Assign (var, expr) -> tr_assign var expr ~ctx
    | If (cond, t, f) -> tr_cond cond t f ~ctx
    | While (cond, body) -> tr_while cond body ~ctx
    | For (_, lo, hi, body, _) -> tr_for lo hi body ~ctx
    | Break br -> tr_break br ~ctx
    | Let (decs, body) -> tr_let decs body ~ctx
    | Array (ty, size, init) -> tr_array ty size init ~ctx

  and tr_break br ~ctx =
    (* List.iteri ctx.path ~f:(fun idx node ->
     *     let expr_str = Syntax.show_expr node.L.value in
     *     print_endline @@ sprintf "\n[%d]:\n\t%s\n" idx expr_str); *)

    let is_loop expr = Syntax.is_loop expr.L.value in
    match List.find ctx.path ~f:is_loop with
    | Some _ ->
      mk_ty T.Unit
    | None ->
      syntax_error br "unexpected break statement"

  and tr_call f args ~ctx =
    match S.Table.find_fun f ctx.venv with
    | Env.VarEntry t ->
      type_error f @@ sprintf
        "expected function, but found variable \"%s\" of type \"%s\""
        (f.L.value.S.name) (T.to_string t)
    | Env.FunEntry (formals, result) ->
      (* check if all the arguments are supplied *)
      if List.length formals <> List.length args then
        type_error f @@ sprintf
          "function \"%s\" expects %d formal arguments, but %d was given"
          (f.L.value.S.name) (List.length formals) (List.length args) ;
      List.iter2_exn formals args ~f:(assert_ty ~ctx);
      T.(mk_ty ~!result)

  (* in our language binary operators work only with
     integer operands, except for (=) and (<>) *)
  and tr_op expr l r ~ctx = function
    | Syntax.Eq | Syntax.Neq ->
      assert_comparison expr l r ~ctx
    | _ ->
      assert_op l r ~ctx

  and assert_comparison expr l r ~ctx =
    let open T in
    let { ty = ty_l; _ } = tr_expr l ~ctx in
    let { ty = ty_r; _ } = tr_expr r ~ctx in
    if ~!ty_l <> ~!ty_r
    then type_mismatch_error3 expr ty_l ty_r;
    mk_ty ty_l

  and assert_op l r ~ctx =
    assert_int l ~ctx;
    assert_int r ~ctx;
    mk_ty T.Int

  and tr_assign var expr ~ctx =
    let { ty = var_ty; _ } = tr_var var ~ctx in
    let { ty = expr_ty; _ } = tr_expr expr ~ctx in
    if T.(var_ty = expr_ty)
    then mk_ty var_ty
    else type_error expr @@ sprintf
        "invalid assigment of type \"%s\" to a variable of type \"%s\""
        (T.to_string expr_ty) (T.to_string var_ty)

  (* type of a sequence is a type of its last expression,
     but we need to check all previous expressions too *)
  and tr_seq exprs ~ctx =
    List.fold_left
      ~f:(fun _ expr -> tr_expr expr ~ctx)
      ~init:(mk_ty T.Unit)
      exprs

  and tr_cond cond t f ~ctx =
    assert_int cond ~ctx;
    let { ty = t_ty; _ } = tr_expr t ~ctx in
    match f with
    | None ->
      mk_ty t_ty
    | Some f ->
      (* If there is a false-branch then we should
         check if types of both branches match *)
      let { ty = f_ty; _ } = tr_expr f ~ctx in
      if T.(t_ty = f_ty)
      then mk_ty t_ty
      else type_error expr @@ sprintf
          "different types of branch expressions: \"%s\" and \"%s\""
          (T.to_string t_ty) (T.to_string f_ty)

  and tr_while cond body ~ctx =
    assert_int cond ~ctx;
    assert_unit body ~ctx;
    mk_ty T.Unit

  and tr_for lo hi body ~ctx =
    assert_int lo ~ctx;
    assert_int hi ~ctx;
    assert_unit body ~ctx;
    mk_ty T.Unit

  (* in Tiger declarations appear only in a "let" expression,
     the let expression modifies both:
     type-level (tenv) and term-level (venv) environments *)
  and tr_let decs body ~ctx =
    (* update env's according to declarations *)
    let ctx = trans_decs decs ~ctx in
    (* then translate the body expression using
       the new augmented environments *)
    trans_expr body ~ctx
    (* then the new environments are discarded *)

  and tr_record_field rec_typ tfields (name, expr) ~ctx =
    (* find a type of the field with [name] *)
    match List.Assoc.find tfields ~equal:S.equal name.L.value with
    | Some ty_field ->
      let { ty = ty_expr; _ } = tr_expr expr ~ctx in
      if not (T.assignable ty_field ty_expr)
      then type_mismatch_error3 expr ty_field ty_expr
    | None ->
      missing_field_error rec_typ name

  and tr_record ty_name vfields ~ctx =
    let open T in
    (* get the record type definition *)
    let rec_typ = S.Table.find_ty ty_name ctx.tenv in
    match ~!rec_typ with
    | T.Record (tfields, _) ->
      (* we want to check each field of the variable
         against the corresponding record type definition *)
      List.iter ~f:(tr_record_field rec_typ tfields ~ctx) vfields;
      mk_ty rec_typ
    | _ ->
      type_error ty_name @@ sprintf
        "\"%s\" is not a record" (T.to_string rec_typ)

  and tr_array typ size init ~ctx =
    let open T in
    assert_int size ~ctx;
    let { ty = init_tn; _ } = tr_expr init ~ctx in
    (* find the type of this particular array *)
    let arr_ty = S.Table.find_ty typ ctx.tenv in
    match ~!arr_ty with
    | T.Array (tn, _) ->
      let t = ~!tn in
      let init_t = ~!init_tn in
      if t = init_t
      then mk_ty arr_ty
      else type_mismatch_error4
          "invalid type of array initial value, " init t init_t
    | _ ->
      type_error typ @@ sprintf
        "\"%s\" is not array" (T.to_string arr_ty)

  and tr_var var ~ctx =
    match var.L.value with
    | SimpleVar var ->
      tr_simple_var var ~ctx
    | FieldVar (var, field) ->
      tr_field_var var field ~ctx
    | SubscriptVar (var, sub) ->
      tr_subscript_var var sub ~ctx

  and tr_simple_var var ~ctx =
    match S.Table.find_var var ctx.venv with
    | Env.VarEntry tn ->
      T.(mk_ty ~!tn)
    | Env.FunEntry (formals, result) ->
      let signature =
        formals
        |> List.map ~f:T.to_string
        |> String.concat ~sep:", " in
      type_error var @@ sprintf
        "expected variable, but found a function \"(%s) : %s\""
        signature (T.to_string result)

  and tr_field_var var field ~ctx =
    (* TODO: check for "nil" *)

    (* find a type of the record variable *)
    let rec_ty = tr_var var ~ctx in
    (* lets see if its actually a record *)
    match rec_ty.ty with
    | Record (fields, _) ->
      (* record is just a list of pairs [S.t * Type.t],
         lets try to find a type for the given [field],
         it is the type of the [FieldVar] expression  *)
      (match List.Assoc.find fields ~equal:S.equal field.L.value with
       | Some tt -> T.(mk_ty ~!tt)
       | None -> missing_field_error rec_ty.ty field)
    | _ ->
      type_error var @@ sprintf
        "expected record, but \"%s\" found"
        (T.to_string rec_ty.ty)

  and tr_subscript_var var sub ~ctx =
    let arr_ty = tr_var var ~ctx in
    match arr_ty.ty with
    | Array (tn, _) ->
      assert_int sub ~ctx;
      T.(mk_ty ~!tn)
    | _ ->
      type_error var @@ sprintf
        "\"%s\" is not an array" (T.to_string arr_ty.ty)

  in tr_expr expr ~ctx

and trans_decs ~ctx =
  List.fold_left
    ~f:(fun ctx dec -> trans_dec dec ~ctx)
    ~init:ctx

(* modifies and returns term-level and
   type-level environments adding the given declaration *)
and trans_dec ~ctx = function
  | TypeDec tys -> trans_tys tys ~ctx
  | FunDec fs -> trans_funs fs ~ctx
  | VarDec var -> trans_var var ~ctx

and trans_tys tys ~ctx =
  let open Syntax in
  let tr_ty_head (tns, tenv) ty_dec =
    let sym = ty_dec.L.value.type_name.L.value in
    let tn = ref None in
    (* add a type-reference, without body *)
    let tenv' = S.Table.set tenv ~key:sym ~data:(T.Name (sym, tn)) in
    tn :: tns, tenv' in
  (* first, we add all the type names to the [tenv] *)
  let (tns, tenv') = List.fold_left tys ~init:([], ctx.tenv) ~f:tr_ty_head in
  let resolve_ty tn ty_dec =
    let { typ; _ } = ty_dec.L.value in
    (* resolve the type body *)
    let t = trans_ty tenv' typ in
    tn := Some t in
  (* resolve the (possibly mutually recursive) types *)
  List.iter2_exn (List.rev tns) tys ~f:resolve_ty;
  { ctx with tenv = tenv' }

and trans_funs fs ~ctx =
  let open Syntax in
  let open Env in
  let tr_fun_head (sigs, venv) fun_dec =
    let { fun_name; params; result_typ; _ } = fun_dec.L.value in
    (* translate function arguments *)
    let tr_arg f = f.name.L.value, S.Table.find_ty f.typ ctx.tenv in
    let args = List.map params ~f:tr_arg in
    let formals = List.map args ~f:snd in
    (* translate the result type *)
    let result = match result_typ with
      | None -> T.Unit
      | Some t -> S.Table.find_ty t ctx.tenv in
    let name = fun_name.L.value in
    let entry = FunEntry (formals, result) in
    let venv' = S.Table.set venv ~key:name ~data:entry in
    (args, result) :: sigs, venv' in
  (* first, we add all the function entries to the [venv] *)
  let (sigs, venv') = List.fold_left fs ~f:tr_fun_head ~init:([], ctx.venv) in
  let assert_fun_body (args, result) fun_dec =
    let { body; _ } = fun_dec.L.value in
    (* now, lets build another [venv''] to be used for body processing
       it should have all the arguments in it *)
    let add_var e (name, t) = S.Table.set e ~key:name ~data:(VarEntry t) in
    let venv'' = List.fold_left args ~init:venv' ~f:add_var in
    let ctx' = { ctx with venv = venv'' } in
    let { ty = body_ty; _ } = trans_expr body ~ctx:ctx' in
    if T.(body_ty <> result)
    then type_mismatch_error4
        "type of the body expression doesn't match the declared result type, "
        body result body_ty; in
  (* now, lets check the bodies *)
  List.iter2_exn (List.rev sigs) fs ~f:assert_fun_body;
  { ctx with venv = venv' }

and assert_init var init_ty ~ctx =
  let open Syntax in
  let { var_typ; init; _ } = var.L.value in
  (* lets see if the variable is annotated *)
  match var_typ with
  | None -> ()
  | Some ann_ty ->
    (* check if the init expression has the
       same type as the variable annotation *)
    let var_ty = S.Table.find_ty ann_ty ctx.tenv in
    if not (T.assignable var_ty init_ty)
    then type_mismatch_error3 init var_ty init_ty

and trans_var var ~ctx =
  let open Syntax in
  let { var_name; init; _ } = var.L.value in
  let { ty = init_ty; _ } = trans_expr init ~ctx in
  assert_init var init_ty ~ctx;
  (* add a new var to the term-level env *)
  let entry = Env.VarEntry init_ty in
  let venv' = S.Table.set ctx.venv ~key:var_name.L.value ~data:entry in
  { ctx with venv = venv' }

(* translates AST type expression into a
   digested type description that we keep in the [tenv] *)
and trans_ty tenv typ =
  let open Syntax in
  match typ with
  | NameTy t ->
    S.Table.find_ty t tenv
  | RecordTy dec_fields ->
    let to_field { name; typ; _ } =
      name.L.value, S.Table.find_ty typ tenv in
    let ty_fields = List.map dec_fields ~f:to_field  in
    T.Record (ty_fields, U.mk ())
  | ArrayTy t ->
    T.Array (S.Table.find_ty t tenv, U.mk ())
