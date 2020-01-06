open Core_kernel
open Err

module T = Type
module L = Location
module U = Unique
module S = Symbol

type tenv = Type.t S.Table.t
type venv = Env.entry S.Table.t

type expr = {
  expr : Translate.expr;
  ty : Type.t;
}

let mk ty = { ty; expr = () }

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

let rec trans_prog expr =
  let open Env in
  ignore @@ trans_expr base
    Translate.outermost (L.dummy expr)

and trans_expr venv tenv level expr =
  let open Syntax in

  let rec assert_ty t expr ~lev =
    let open T in
    let { ty; _ } = tr_expr lev expr in
    if ~!ty <> ~!t
    then type_mismatch_error3 expr t ty

  and assert_int lev expr = assert_ty T.Int expr ~lev
  and assert_unit lev expr = assert_ty T.Unit expr ~lev

  and tr_expr lev expr =
    match expr.L.value with
    | Var var -> tr_var lev var
    | Nil _ -> mk T.Nil
    | Int _ -> mk T.Int
    | String _ -> mk T.String
    | Call (f, args) -> tr_call lev f args
    | Op (l, op, r) -> tr_op lev expr l r op.L.value
    | Record (name, fields) -> tr_record lev name fields
    | Seq exprs -> tr_seq lev exprs
    | Assign (var, expr) -> tr_assign lev var expr
    | If (cond, t, f) -> tr_cond lev cond t f
    | While (cond, body) -> tr_while lev cond body
    | For (_, lo, hi, body, _) -> tr_for lev lo hi body
    | Break _ -> mk T.Unit
    | Let (decs, body) -> tr_let lev decs body
    | Array (ty, size, init) -> tr_array lev ty size init

  and tr_call lev f args =
    match S.Table.find_fun f venv with
    | Env.VarEntry { ty; _ } ->
      type_error f @@ sprintf
        "expected function, but found variable \"%s\" of type \"%s\""
        (f.L.value.S.name) (T.to_string ty)
    | Env.FunEntry { formals; result; _ } ->
      (* check if all the arguments are supplied *)
      if List.length formals <> List.length args then
        type_error f @@ sprintf
          "function \"%s\" expects %d formal arguments, but %d was given"
          (f.L.value.S.name) (List.length formals) (List.length args) ;
      List.iter2_exn formals args ~f:(assert_ty ~lev);
      T.(mk ~!result)

  (* in our language binary operators work only with
   * integer operands, except for (=) and (<>) *)
  and tr_op lev expr l r op =
    match op with
    | Syntax.Eq | Syntax.Neq ->
      assert_comparison lev expr l r
    | _ ->
      assert_op lev l r

  and assert_comparison lev expr l r =
    let open T in
    let { ty = ty_l; _ } = tr_expr lev l in
    let { ty = ty_r; _ } = tr_expr lev r in
    if ~!ty_l <> ~!ty_r
    then type_mismatch_error3 expr ty_l ty_r;
    mk ty_l

  and assert_op lev l r =
    assert_int lev l;
    assert_int lev r;
    mk T.Int

  and tr_assign lev var expr =
    let { ty = var_ty; _ } = tr_var lev var in
    let { ty = expr_ty; _ } = tr_expr lev expr in
    if T.(var_ty = expr_ty)
    then mk var_ty
    else type_error expr @@ sprintf
        "invalid assigment of type \"%s\" to a variable of type \"%s\""
        (T.to_string expr_ty) (T.to_string var_ty)

  (* type of a sequence is a type of its last expression,
   * but we need to check all previous expressions too *)
  and tr_seq lev exprs =
    List.fold_left
      ~f:(fun _ expr -> tr_expr lev expr)
      ~init:(mk T.Unit)
      exprs

  and tr_cond lev cond t f =
    assert_int lev cond;
    let { ty = t_ty; _ } = tr_expr lev t in
    match f with
    | None ->
      mk t_ty
    | Some f ->
      (* If there is a false-branch then we should
       * check if types of both branches match *)
      let { ty = f_ty; _ } = tr_expr lev f in
      if T.(t_ty = f_ty)
      then mk t_ty
      else type_error expr @@ sprintf
          "different types of branch expressions: \"%s\" and \"%s\""
          (T.to_string t_ty) (T.to_string f_ty)

  and tr_while lev cond body =
    assert_int lev cond;
    assert_unit lev body;
    mk T.Unit

  and tr_for lev lo hi body =
    assert_int lev lo;
    assert_int lev hi;
    assert_unit lev body;
    mk T.Unit

  (* in Tiger declarations appear only in a "let" expression,
     the let expression modifies both:
     type-level (tenv) and term-level (value-level/venv) environments *)
  and tr_let lev decs body =
    (* update env's according to declarations *)
    let venv', tenv' = trans_decs venv tenv lev decs in
    (* then mk the body expression using
       the new augmented environments *)
    trans_expr venv' tenv' lev body
  (* then the new environments are discarded *)

  and tr_record_field lev rec_typ tfields (name, expr) =
    (* find a type of the field with [name] *)
    match List.Assoc.find tfields ~equal:S.equal name.L.value with
    | Some tvn ->
      let open T in
      let { ty = tn; _ } = tr_expr lev expr in
      let ty = ~!tn in
      let tv = ~!tvn in
      if ty <> tv then type_mismatch_error3 expr ty tv
    | None ->
      missing_field_error rec_typ name

  and tr_record lev ty_name vfields =
    let open T in
    (* get the record type definition *)
    let rec_typ = S.Table.find_ty ty_name tenv in
    match T.actual rec_typ with
    | T.Record (tfields, _) ->
      (* we want to check each field of the variable
       * against the corresponding record type definition *)

      List.iter ~f:(tr_record_field lev rec_typ tfields) vfields;
      mk rec_typ
    | _ ->
      type_error ty_name @@ sprintf
        "\"%s\" is not a record" (T.to_string rec_typ)

  and tr_array lev typ size init =
    let open T in
    assert_int lev size;
    let { ty = init_tn; _ } = tr_expr lev init in
    (* find the type of this particular array *)
    let arr_ty = S.Table.find_ty typ tenv in
    match T.actual arr_ty with
    | T.Array (tn, _) ->
      let t = T.actual tn in
      let init_t = ~!init_tn in
      if t = init_t
      then mk arr_ty
      else type_mismatch_error4
          "invalid type of array initial value, " init t init_t
    | _ ->
      type_error typ @@ sprintf
        "\"%s\" is not array" (T.to_string arr_ty)

  and tr_var lev var =
    match var.L.value with
    | SimpleVar var ->
      tr_simple_var var
    | FieldVar (var, field) ->
      tr_field_var lev var field
    | SubscriptVar (var, sub) ->
      tr_subscript_var lev var sub

  and tr_simple_var var =
    match S.Table.find_var var venv with
    | Env.VarEntry { ty; _ }  ->
      T.(mk ~!ty)
    | Env.FunEntry { formals; result; _ } ->
      let signature =
        formals
        |> List.map ~f:T.to_string
        |> String.concat ~sep:", " in
      type_error var @@ sprintf
        "expected variable, but found a function \"(%s) : %s\""
        signature (T.to_string result)

  and tr_field_var lev var field =
    (* find a type of the record variable *)
    let rec_ty = tr_var lev var in
    (* lets see if its actually a record *)
    match rec_ty.ty with
    | Record (fields, _) ->
      (* record is just a list of pairs (S.t * Type.t),
         lets try to find a type for the given [field],
         it is the type of the [FieldVar] expression  *)
      (match List.Assoc.find fields ~equal:S.equal field.L.value with
       | Some tt -> T.(mk ~!tt)
       | None -> missing_field_error rec_ty.ty field)
    | _ ->
      type_error var @@ sprintf
        "expected record, but \"%s\" found"
        (T.to_string rec_ty.ty)

  and tr_subscript_var lev var sub =
    let arr_ty = tr_var lev var in
    match arr_ty.ty with
    | Array (tn, _) ->
      assert_int lev sub;
      mk (T.actual tn)
    | _ ->
      type_error var @@ sprintf
        "\"%s\" is not an array" (T.to_string arr_ty.ty)

  in tr_expr level expr

and trans_decs venv tenv lev =
  List.fold_left
    ~f:(fun (venv, tenv) dec -> trans_dec venv tenv lev dec)
    ~init:(venv, tenv)

(* modifies and returns term-level and
 * type-level environments adding the given declaration *)
and trans_dec venv tenv lev dec =
  let open Syntax in
  match dec with
  | TypeDec tys -> trans_tys venv tenv tys
  | FunDec fs -> trans_funs venv tenv lev fs
  | VarDec var -> trans_var venv tenv lev var

and trans_tys venv tenv tys =
  let open Syntax in
  let tr_ty_head (tns, tenv) ty_dec =
    let sym = ty_dec.L.value.type_name.L.value in
    let tn = ref None in
    (* add a type-reference, without body *)
    let tenv' = S.Table.add_exn tenv ~key:sym ~data:(T.Name (sym, tn)) in
    tn :: tns, tenv' in
  (* first, we add all the type names to the [tenv] *)
  let (tns, tenv') = List.fold_left tys ~init:([], tenv) ~f:tr_ty_head in
  let resolve_ty tn ty_dec =
    let { typ; _ } = ty_dec.L.value in
    (* resolve the type body *)
    let t = trans_ty tenv' typ in
    tn := Some t in
  (* resolve the (possibly mutually recursive) types *)
  List.iter2_exn (List.rev tns) tys ~f:resolve_ty;
  venv, tenv'

(* translates (possibly mutually-recursive) functions,
   each of which creates a new "nesting level" *)
and trans_funs venv tenv lev fs =
  let open Syntax in
  let open Env in
  (* translates a function declaration and updates the [venv] *)
  let tr_decl (sigs, venv) fun_dec =
    let { fun_name; params; result_typ; _ } = fun_dec.L.value in
    (* mk function arguments *)
    let args = List.map params ~f:(fun f -> f.name.L.value, S.Table.find_ty f.typ tenv) in
    (* mk the result type *)
    let result = match result_typ with
      | None -> T.Unit
      | Some t -> S.Table.find_ty t tenv in
    (* lets just assume that everything escapes for now (as adviced in ch6),
       the next step would be to lookup those in the [Escape.env] *)
    let esc_formals = List.map args ~f:(fun _ -> true) in
    (* generate a new label *)
    let label = Temp.mk_label None in
    (* create a new "nesting level" *)
    let level = Translate.mk (Some lev) label esc_formals in
    (* get types of the formal parameters *)
    let formals = List.map args ~f:snd in
    let entry = FunEntry { level; label; formals; result } in
    let venv' = S.Table.add_exn venv ~key:fun_name.L.value ~data:entry in
    (args, result) :: sigs, venv' in

  (* first, we add all the function entries to the [venv] *)
  let (sigs, venv') = List.fold_left fs ~f:tr_decl ~init:([], venv) in

  let assert_body (args, result) fun_dec =
    (* now, lets build another [venv''] to be used for body processing
     * it should have all the arguments in it *)
    let add_var e (name, ty) =
      (* lets just assume that each variable escapes for now *)
      let access = Translate.alloc_local lev true in
      let entry = VarEntry { ty; access } in
      S.Table.add_exn e ~key:name ~data:entry
    in
    let venv'' = List.fold_left args ~f:add_var ~init:venv' in
    let { body; _ } = fun_dec.L.value in
    let body_te = trans_expr venv'' tenv lev body in
    if T.(body_te.ty <> result)
    then type_mismatch_error4
        "type of the body expression doesn't match the declared result type, "
        body result body_te.ty; in

  (* now, lets asssert the bodies *)
  List.iter2_exn (List.rev sigs) fs ~f:assert_body;
  venv', tenv

and trans_var venv tenv lev var =
  let open Syntax in
  let { var_name; var_typ; init; _ } = var.L.value in
  let { ty = init_ty; _ } = trans_expr venv tenv lev init in
  begin
    (* lets see if the variable is annotated *)
    match var_typ with
    | None -> ()
    | Some ann_ty ->
      let open T in
      (* check if the init expression has the
       * same type as the variable annotation *)
     let var_ty = ~!(S.Table.find_ty ann_ty tenv) in
     if var_ty <> ~!init_ty
     then type_mismatch_error3 init var_ty init_ty
  end;
  let access = Translate.alloc_local lev true in
  let entry = Env.VarEntry { ty = init_ty; access } in
  let venv' = S.Table.add_exn venv ~key:var_name.L.value ~data:entry in
  venv', tenv

(* translates an AST type expression into a
 * digested type description that we keep in the [tenv] *)
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
