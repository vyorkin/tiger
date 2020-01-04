open Core_kernel
open Err

module T = Type
module L = Location
module U = Unique
module S = Symbol
module ST = Symbol_table

type expr_ty =
  { expr : Translate.t;
    ty : T.t;
  }

let ret ty =
  Trace.Semant.ret_ty ty;
  { expr = (); ty }

let ret_int = ret T.Int
let ret_string = ret T.String
let ret_nil = ret T.Nil
let ret_unit = ret T.Unit

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
  Trace.Semant.trans_prog expr;
  ignore @@ trans_expr (L.dummy expr) ~env:(Env.mk ())

and trans_expr expr ~env =
  let open Syntax in
  let open Env in

  let rec assert_ty t expr ~env =
    Trace.Semant.assert_ty t expr;
    let { ty; _ } = tr_expr expr ~env in
    if T.(~!ty <> ~!t)
    then type_mismatch_error3 expr t ty

  and assert_int expr ~env = assert_ty T.Int expr ~env
  and assert_unit expr ~env = assert_ty T.Unit expr ~env

  and tr_expr expr ~env =
    Trace.Semant.tr_expr expr;
    (* Push the current [expr] to the [path] stack *)
    let env = Env.enter_expr env expr in
    match expr.L.value with
    | Var var -> tr_var var ~env
    | Nil _ -> ret_nil
    | Int _ -> ret_int
    | String _ -> ret_string
    | Call (f, args) -> tr_call f args ~env
    | Op (l, op, r) -> tr_op expr l r op.L.value ~env
    | Record (name, fields) -> tr_record name fields ~env
    | Seq [] -> ret_unit (* in our grammar unit is empty seq *)
    | Seq exprs -> tr_seq exprs ~env
    | Assign (var, expr) -> tr_assign var expr ~env
    | If (cond, t, f) -> tr_cond cond t f ~env
    | While (cond, body) -> tr_while cond body ~env
    | For (var, lo, hi, body, _) -> tr_for var lo hi body ~env
    | Break br -> tr_break br ~env
    | Let (decs, body) -> tr_let decs body ~env
    | Array (ty, size, init) -> tr_array ty size init ~env

  and tr_break br ~env =
    Trace.Semant.tr_break br env.loop;
    match env.loop with
    | Some _ ->
      (* TODO: Trace breaking the loop *)
      ret_unit
    | None ->
      syntax_error br "unexpected break statement"

  and tr_call f args ~env =
    Trace.Semant.tr_call f args;
    match ST.look_fun env.venv f with
    | Env.VarEntry t ->
      type_error f @@ sprintf
        "expected function, but found variable \"%s\" of type \"%s\""
        (f.L.value.S.name) (T.to_string t)
    | Env.FunEntry (formals, result) ->
      (* Check if all the arguments are supplied *)
      if List.length formals <> List.length args then
        type_error f @@ sprintf
          "function \"%s\" expects %d formal arguments, but %d was given"
          (f.L.value.S.name) (List.length formals) (List.length args) ;
      List.iter2_exn formals args ~f:(assert_ty ~env);
      T.(ret ~!result)

  (* In our language binary operators work only with
     integer operands, except for (=) and (<>) *)
  and tr_op expr l r op ~env =
    Trace.Semant.tr_op l r op;
    match op with
    | Syntax.Eq | Syntax.Neq ->
      assert_comparison expr l r ~env
    | _ ->
      assert_op l r ~env

  and assert_comparison expr l r ~env =
    let { ty = ty_l; _ } = tr_expr l ~env in
    let { ty = ty_r; _ } = tr_expr r ~env in
    if T.(~!ty_l <> ~!ty_r)
    then type_mismatch_error3 expr ty_l ty_r;
    ret ty_l

  and assert_op l r ~env =
    assert_int l ~env;
    assert_int r ~env;
    ret_int

  and tr_assign var expr ~env =
    Trace.Semant.tr_assign var expr;
    let { ty = var_ty; _ } = tr_var var ~env in
    let { ty = expr_ty; _ } = tr_expr expr ~env in
    if T.(var_ty = expr_ty)
    then ret var_ty
    else type_error expr @@ sprintf
        "invalid assigment of type \"%s\" to a variable of type \"%s\""
        (T.to_string expr_ty) (T.to_string var_ty)

  (* Type of a sequence is a type of its last expression,
     but we need to check all previous expressions too *)
  and tr_seq exprs ~env =
    Trace.Semant.tr_seq exprs;
    List.fold_left
      ~f:(fun _ expr -> tr_expr expr ~env)
      ~init:ret_unit
      exprs

  and tr_cond cond t f ~env =
    Trace.Semant.tr_cond cond t f;
    assert_int cond ~env;
    let { ty = t_ty; _ } = tr_expr t ~env in
    match f with
    | None ->
      ret t_ty
    | Some f ->
      (* If there is a false-branch then we should
         check if types of both branches match *)
      let { ty = f_ty; _ } = tr_expr f ~env in
      if T.(t_ty = f_ty)
      then ret t_ty
      else type_error expr @@ sprintf
          "different types of branch expressions: \"%s\" and \"%s\""
          (T.to_string t_ty) (T.to_string f_ty)

  and tr_while cond body ~env =
    Trace.Semant.tr_while cond body;
    assert_int cond ~env;
    assert_unit body ~env:(Env.enter_loop env "while");
    ret_unit

  and tr_for var lo hi body ~env =
    Trace.Semant.tr_for var lo hi body;
    assert_int lo ~env;
    assert_int hi ~env;
    (* Add iterator var to the term-level env  *)
    let entry = Env.VarEntry T.Int in
    let venv' = ST.bind_var env.venv var entry in
    let env = Env.enter_loop { env with venv = venv' } "for" in
    assert_unit body ~env;
    ret_unit

  (* In Tiger declarations appear only in a "let" expression,
     the let expression modifies both:
     type-level (tenv) and term-level (venv) environments *)
  and tr_let decs body ~env =
    Trace.Semant.tr_let decs body;
    (* Update env's according to declarations *)
    let env' = trans_decs decs ~env in
    (* Then translate the body expression using
       the new augmented environments *)
    trans_expr body ~env:env'
  (* Then the new environments are discarded *)

  and tr_record_field rec_typ tfields (name, expr) ~env =
    Trace.Semant.tr_record_field rec_typ name expr;
    (* Find a type of the field with [name] *)
    match List.Assoc.find tfields ~equal:S.equal name.L.value with
    | Some ty_field ->
      let { ty = ty_expr; _ } = tr_expr expr ~env in
      if T.(ty_field @<> ty_expr)
      then type_mismatch_error3 expr ty_field ty_expr
    | None ->
      missing_field_error rec_typ name

  and tr_record ty_name vfields ~env =
    let open T in
    Trace.Semant.tr_record ty_name vfields;
    (* Get the record type definition *)
    let rec_typ = ST.look_typ env.tenv ty_name in
    match ~!rec_typ with
    | T.Record (tfields, _) ->
      (* We want to check each field of the variable
         against the corresponding record type definition *)
      List.iter ~f:(tr_record_field rec_typ tfields ~env) vfields;
      ret rec_typ
    | _ ->
      type_error ty_name @@ sprintf
        "\"%s\" is not a record" (T.to_string rec_typ)

  and tr_array typ size init ~env =
    let open T in
    Trace.Semant.tr_array typ size init;
    assert_int size ~env;
    let { ty = init_tn; _ } = tr_expr init ~env in
    (* Find the type of this particular array *)
    let arr_ty = ST.look_typ env.tenv typ in
    match ~!arr_ty with
    | T.Array (tn, _) ->
      let t = ~!tn in
      let init_t = ~!init_tn in
      if t = init_t
      then ret arr_ty
      else type_mismatch_error4
          "invalid type of array initial value, " init t init_t
    | _ ->
      type_error typ @@ sprintf
        "\"%s\" is not array" (T.to_string arr_ty)

  and tr_var var ~env =
    Trace.Semant.tr_var var;
    match var.L.value with
    | SimpleVar var ->
      tr_simple_var var ~env
    | FieldVar (var, field) ->
      tr_field_var var field ~env
    | SubscriptVar (var, sub) ->
      tr_subscript_var var sub ~env

  and tr_simple_var var ~env =
    Trace.Semant.tr_simple_var var;
    match ST.look_var env.venv var with
    | Env.VarEntry tn ->
      T.(ret ~!tn)
    | Env.FunEntry (formals, result) ->
      let signature =
        formals
        |> List.map ~f:T.to_string
        |> String.concat ~sep:", " in
      type_error var @@ sprintf
        "expected variable, but found a function \"(%s) : %s\""
        signature (T.to_string result)

  and tr_field_var var field ~env =
    Trace.Semant.tr_field_var var field;
    (* TODO: Check for "nil" *)

    (* Find a type of the record variable *)
    let rec_ty = tr_var var ~env in
    (* Lets see if its actually a record *)
    match rec_ty.ty with
    | Record (fields, _) ->
      (* Record is just a list of pairs [S.t * Type.t],
         lets try to find a type for the given [field],
         it is the type of the [FieldVar] expression  *)
      (match List.Assoc.find fields ~equal:S.equal field.L.value with
       | Some tt -> T.(ret ~!tt)
       | None -> missing_field_error rec_ty.ty field)
    | _ ->
      type_error var @@ sprintf
        "expected record, but \"%s\" found"
        (T.to_string rec_ty.ty)

  and tr_subscript_var var sub ~env =
    Trace.Semant.tr_subscript_var var sub;
    let arr_ty = tr_var var ~env in
    match arr_ty.ty with
    | Array (tn, _) ->
      assert_int sub ~env;
      T.(ret ~!tn)
    | _ ->
      type_error var @@ sprintf
        "\"%s\" is not an array" (T.to_string arr_ty.ty)

  in
  tr_expr expr ~env

and trans_decs decs ~env =
  Trace.Semant.trans_decs decs;
  List.fold_left decs
    ~f:(fun env dec -> trans_dec dec ~env)
    ~init:env

(* Modifies and returns term-level and
   type-level environments adding the given declaration *)
and trans_dec ~env = function
  | TypeDec tys -> trans_tys tys ~env
  | FunDec fs -> trans_funs fs ~env
  | VarDec var -> trans_var var ~env

and trans_tys tys ~env =
  let open Syntax in
  let tr_ty_head (tns, tenv) ty_dec =
    let typ = ty_dec.L.value.type_name in
    let tn = ref None in
    (* Add a type-reference, without body *)
    let tenv' = ST.bind_typ tenv typ (T.Name (typ.L.value, tn)) in
    tn :: tns, tenv' in
  (* First, we add all the type names to the [tenv] *)
  let (tns, tenv') = List.fold_left tys ~init:([], env.tenv) ~f:tr_ty_head in
  let resolve_ty tn ty_dec =
    let { typ; _ } = ty_dec.L.value in
    (* Resolve the type body *)
    let t = trans_ty tenv' typ in
    tn := Some t
  in
  Trace.Semant.trans_tys tys;
  (* Resolve the (possibly mutually recursive) types *)
  List.iter2_exn (List.rev tns) tys ~f:resolve_ty;
  { env with tenv = tenv' }

and trans_funs fs ~env =
  let open Syntax in
  let open Env in
  let tr_fun_head (sigs, venv) fun_dec =
    Trace.Semant.trans_fun_head fun_dec;
    let { fun_name; params; result_typ; _ } = fun_dec.L.value in
    (* Translate function arguments *)
    let tr_arg f = f.name, ST.look_typ env.tenv f.typ in
    let args = List.map params ~f:tr_arg in
    let formals = List.map args ~f:snd in
    (* Translate the result type *)
    let result = match result_typ with
      | None -> T.Unit
      | Some t -> ST.look_typ env.tenv t in
    let entry = FunEntry (formals, result) in
    let venv' = ST.bind_fun venv fun_name entry in
    (args, result) :: sigs, venv' in
  (* First, we add all the function entries to the [venv] *)
  let (sigs, venv') = List.fold_left fs ~f:tr_fun_head ~init:([], env.venv) in
  let assert_fun_body (args, result) fun_dec =
    Trace.Semant.assert_fun_body fun_dec result;
    let { body; _ } = fun_dec.L.value in
    (* Here, we build another [venv''] to be used for body processing
       it should have all the arguments in it *)
    let add_var e (name, t) = ST.bind_var e name (VarEntry t) in
    let venv'' = List.fold_left args ~init:venv' ~f:add_var in
    let env' = { env with venv = venv'' } in
    let { ty = body_ty; _ } = trans_expr body ~env:env' in
    if T.(body_ty <> result)
    then type_mismatch_error4
        "type of the body expression doesn't match the declared result type, "
        body result body_ty;
  in
  Trace.Semant.trans_funs fs;
  (* Now, lets check the bodies *)
  List.iter2_exn (List.rev sigs) fs ~f:assert_fun_body;
  { env with venv = venv' }

and assert_init var init_ty ~env =
  let open Syntax in
  let open Env in
  let { var_typ; init; _ } = var.L.value in
  (* Lets see if the variable is annotated *)
  match var_typ with
  | None -> ()
  | Some ann_ty ->
    (* Check if the init expression has the
       same type as the variable annotation *)
    let var_ty = ST.look_typ env.tenv ann_ty in
    if T.(var_ty @<> init_ty)
    then type_mismatch_error3 init var_ty init_ty

and trans_var var ~env =
  let open Syntax in
  Trace.Semant.trans_var var;
  let { var_name; init; _ } = var.L.value in
  let { ty = init_ty; _ } = trans_expr init ~env in
  assert_init var init_ty ~env;
  (* Add a new var to the term-level env *)
  let entry = Env.VarEntry init_ty in
  let venv' = ST.bind_var env.venv var_name entry in
  { env with venv = venv' }

(* Translates AST type expression into a
   digested type description that we keep in the [tenv] *)
and trans_ty tenv typ =
  let open Syntax in
  Trace.Semant.trans_ty typ;
  match typ with
  | NameTy t ->
    ST.look_typ tenv t
  | RecordTy dec_fields ->
    let to_field { name; typ; _ } =
      name.L.value, ST.look_typ tenv typ in
    let ty_fields = List.map dec_fields ~f:to_field in
    T.Record (ty_fields, U.mk ())
  | ArrayTy t ->
    T.Array (ST.look_typ tenv t, U.mk ())
