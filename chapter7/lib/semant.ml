open Core_kernel
open Err

module T = Type
module Tr = Translate
module L = Location
module U = Unique
module S = Symbol
module ST = Symbol_table

(* This module shouldn't contain any direct
   reference to the [Ir] or [Frame] modules.
   Any manipulation of [Ir] trees should be
   done by [Translate] module *)

type expr_ty = {
  expr : Tr.expr;
  ty : T.t;
}

let ret expr ty =
  Trace.SemanticAnalysis.ret expr ty;
  { expr; ty }

let e_dummy = Tr.e_dummy ()

let ret_int = ret e_dummy T.Int
let ret_string = ret e_dummy T.String
let ret_nil = ret e_dummy T.Nil
let ret_unit = ret e_dummy T.Unit

let type_mismatch_error4 msg l expected actual =
  let msg' = sprintf
      "type \"%s\" is expected, but found \"%s\""
      (T.to_string expected) (T.to_string actual) in
  type_error l @@ msg ^ msg'

let type_mismatch_error3 l expected actual =
  type_mismatch_error4 "" l expected actual

let missing_field_error t name =
  id_error name @@ sprintf
    "record of type \"%s\" doesn't have field \"%s\""
    (T.to_string t) (name.L.value.S.name)

let rec trans_prog expr =
  Trace.SemanticAnalysis.trans_prog expr;
  let env = Env.mk () in
  ignore @@ trans_expr L.(~?expr) ~env

and trans_expr expr ~env =
  let open Syntax in

  let rec assert_ty e a =
    Trace.SemanticAnalysis.assert_ty e a;
    if T.(~!e <> ~!a) then type_mismatch_error3 expr e a

  and assert_ty_tr expected_t expr ~env =
    let { ty = actual_t; _ } = tr_expr expr ~env in
    assert_ty expected_t actual_t

  and assert_int expr ~env = assert_ty_tr T.Int expr ~env
  and assert_unit expr ~env = assert_ty_tr T.Unit expr ~env

  and tr_expr expr ~env =
    Trace.SemanticAnalysis.tr_expr expr;
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
    | For (var, lo, hi, body, escapes) -> tr_for var lo hi body escapes ~env
    | Break br -> tr_break br ~env
    | Let (decs, body) -> tr_let decs body ~env
    | Array (ty, size, init) -> tr_array ty size init ~env

  and tr_break br ~env =
    Trace.SemanticAnalysis.tr_break br env.loop;
    match env.loop with
    | Some _ ->
      ret_unit
    | None ->
      syntax_error br "unexpected break statement"

  and tr_call f args ~env =
    Trace.SemanticAnalysis.tr_call f args;
    match ST.look_fun env.venv f with
    | VarEntry { ty; _ } ->
      type_error f @@ sprintf
        "expected function, but found variable \"%s\" of type \"%s\""
        (f.L.value.S.name) (T.to_string ty)
    | FunEntry { formals; result; _ } ->
      (* Check if all the arguments are supplied *)
      if List.length formals <> List.length args then
        type_error f @@ sprintf
          "function \"%s\" expects %d formal arguments, but %d was given"
          (f.L.value.S.name) (List.length formals) (List.length args) ;
      (* Translate [args] first *)
      let args_r = List.map args ~f:(tr_expr ~env) in
      (* Assert that argument types match types of the function formal parameters *)
      List.iter2_exn formals args_r ~f:(fun t ({ ty; _ }) -> assert_ty t ty);
      T.(ret e_dummy ~!result)

  (* In our language binary operators work only with
     integer operands, except for (=) and (<>) *)
  and tr_op expr l r op ~env =
    Trace.SemanticAnalysis.tr_op l r op;
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
    ret e_dummy ty_l

  and assert_op l r ~env =
    assert_int l ~env;
    assert_int r ~env;
    ret_int

  and tr_assign var expr ~env =
    Trace.SemanticAnalysis.tr_assign var expr;
    let { ty = var_ty; _ } = tr_var var ~env in
    let { ty = expr_ty; _ } = tr_expr expr ~env in
    if T.(var_ty = expr_ty)
    then ret_unit
    else type_error expr @@ sprintf
        "invalid assigment of type \"%s\" to a variable of type \"%s\""
        (T.to_string expr_ty) (T.to_string var_ty)

  (* Type of a sequence is a type of its last expression,
     but we need to check all previous expressions too *)
  and tr_seq exprs ~env =
    Trace.SemanticAnalysis.tr_seq exprs;
    List.fold_left
      ~f:(fun _ expr -> tr_expr expr ~env)
      ~init:ret_unit
      exprs

  and tr_cond cond t f ~env =
    Trace.SemanticAnalysis.tr_cond cond t f;
    assert_int cond ~env;
    Trace.SemanticAnalysis.tr_then ();
    let { ty = t_ty; _ } = tr_expr t ~env in
    match f with
    | None ->
      assert_unit t ~env;
      ret e_dummy t_ty
    | Some f ->
      Trace.SemanticAnalysis.tr_else ();
      (* If there is a false-branch then we should
         check if types of both branches match *)
      let { ty = f_ty; _ } = tr_expr f ~env in
      if T.(t_ty = f_ty)
      then ret e_dummy t_ty
      else type_error expr @@ sprintf
          "different types of branch expressions: \"%s\" and \"%s\""
          (T.to_string t_ty) (T.to_string f_ty)

  and tr_while cond body ~env =
    Trace.SemanticAnalysis.tr_while cond body;
    assert_int cond ~env;
    assert_unit body ~env:(Env.enter_loop env "while");
    ret_unit

  and tr_for var lo hi body escapes ~env =
    Trace.SemanticAnalysis.tr_for var lo hi body escapes;
    (* Let's keep these assertions here to
       fail early and get a better error message *)
    assert_int lo ~env;
    assert_int hi ~env;
    (* rewrite for to let + while *)
    let let_expr = Syntax_rewriter.rewrite_for var lo hi body escapes in
    tr_expr let_expr ~env

  (* In Tiger declarations appear only in a "let" expression,
     the let expression modifies both:
     type-level (tenv) and term-level (value-level/venv) environments *)
  and tr_let decs body ~env =
    Trace.SemanticAnalysis.tr_let decs body;
    (* Update env according to declarations *)
    let env' = trans_decs decs ~env in
    (* Then translate the body expression using
       the new augmented environments *)
    trans_expr body ~env:env'
    (* Then the new environments are discarded *)

  and tr_record_field rec_typ tfields (name, expr) ~env =
    Trace.SemanticAnalysis.tr_record_field name expr rec_typ;
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
    Trace.SemanticAnalysis.tr_record ty_name vfields;
    (* Get the record type definition *)
    let rec_typ = ST.look_typ env.tenv ty_name in
    match ~!rec_typ with
    | T.Record (tfields, _) ->
      (* We want to check each field of the variable
         against the corresponding record type definition *)
      List.iter ~f:(tr_record_field rec_typ tfields ~env) vfields;
      ret e_dummy rec_typ
    | _ ->
      type_error ty_name @@ sprintf
        "\"%s\" is not a record" (T.to_string rec_typ)

  and tr_array typ size init ~env =
    let open T in
    Trace.SemanticAnalysis.tr_array typ size init;
    assert_int size ~env;
    let { ty = init_tn; _ } = tr_expr init ~env in
    (* Find the type of this particular array *)
    let arr_ty = ST.look_typ env.tenv typ in
    match ~!arr_ty with
    | T.Array (tn, _) ->
      let t = ~!tn in
      let init_t = ~!init_tn in
      if t = init_t
      then ret e_dummy arr_ty
      else type_mismatch_error4
          "invalid type of array initial value, " init t init_t
    | _ ->
      type_error typ @@ sprintf
        "\"%s\" is not array" (T.to_string arr_ty)

  and tr_var var ~env =
    Trace.SemanticAnalysis.tr_var var;
    match var.L.value with
    | SimpleVar var ->
      tr_simple_var var ~env
    | FieldVar (var, field) ->
      tr_field_var var field ~env
    | SubscriptVar (var, sub) ->
      tr_subscript_var var sub ~env

  and tr_simple_var var ~env =
    Trace.SemanticAnalysis.tr_simple_var var;
    match ST.look_var env.venv var with
    | VarEntry { ty; _ }  ->
      T.(ret e_dummy ~!ty)
    | FunEntry { formals; result; _ } ->
      let signature =
        formals
        |> List.map ~f:T.to_string
        |> String.concat ~sep:", " in
      type_error var @@ sprintf
        "expected variable, but found a function \"(%s) : %s\""
        signature (T.to_string result)

  and tr_field_var var field ~env =
    Trace.SemanticAnalysis.tr_field_var var field;
    (* Find a type of the record variable *)
    let rec_ty = tr_var var ~env in
    (* Lets see if its actually a record *)
    match rec_ty.ty with
    | Record (fields, _) ->
      (* Record is just a list of pairs [S.t * Type.t],
         lets try to find a type for the given [field],
         it is the type of the [FieldVar] expression  *)
      (match List.Assoc.find fields ~equal:S.equal field.L.value with
       | Some tt -> T.(ret e_dummy ~!tt)
       | None -> missing_field_error rec_ty.ty field)
    | _ ->
      type_error var @@ sprintf
        "expected record, but \"%s\" found"
        (T.to_string rec_ty.ty)

  and tr_subscript_var var sub ~env =
    Trace.SemanticAnalysis.tr_subscript_var var sub;
    let arr_ty = tr_var var ~env in
    match arr_ty.ty with
    | Array (tn, _) ->
      assert_int sub ~env;
      T.(ret e_dummy ~!tn)
    | _ ->
      type_error var @@ sprintf
        "\"%s\" is not an array" (T.to_string arr_ty.ty)
  in
  tr_expr expr ~env

and trans_decs decs ~env =
  Trace.SemanticAnalysis.trans_decs decs;
  List.fold_left decs
    ~f:(fun env dec -> trans_dec dec ~env)
    ~init:env

(* Modifies and returns term-level and
   type-level environments adding the given declaration *)
and trans_dec ~env = function
  | TypeDec tys -> trans_type_decs tys ~env
  | FunDec fs -> trans_fun_decs fs ~env
  | VarDec var -> trans_var_dec var ~env

and trans_type_decs tys ~env =
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
  Trace.SemanticAnalysis.trans_type_decs tys;
  (* Resolve the (possibly mutually recursive) types *)
  List.iter2_exn (List.rev tns) tys ~f:resolve_ty;
  { env with tenv = tenv' }

(* Translates (possibly mutually-recursive) functions,
   each of which creates a new "nesting level" *)
and trans_fun_decs fs ~env =
  let open Syntax in
  let open Env in
  (* Translates a function declaration and updates the [venv] *)
  let tr_fun_head (sigs, venv) fun_dec =
    Trace.SemanticAnalysis.trans_fun_head fun_dec;
    let { fun_name; params; result_typ; _ } = fun_dec.L.value in
    (* Translate function arguments *)
    let tr_param (param : field) = param, ST.look_typ env.tenv param.typ in
    let args = List.map params ~f:tr_param in
    (* Translate the result type *)
    let result = match result_typ with
      | None -> T.Unit
      | Some t -> ST.look_typ env.tenv t in
    let esc_formals = List.map params ~f:(fun f -> !(f.escapes)) in
    (* Generate a new label *)
    let label = Temp.mk_label None in
    (* Create a new "nesting level". *)
    let parent = Some env.level in
    let level = Tr.new_level ~parent ~label ~formals:esc_formals in
    Trace.Translation.new_level level;
    (* Get types of the formal parameters *)
    let formals = List.map args ~f:snd in
    let entry = FunEntry { level; label; formals; result } in
    let venv' = ST.bind_fun venv fun_name entry in
    (level, args, result) :: sigs, venv' in
  (* First, we add all the function entries to the [venv] *)
  let (sigs, venv') = List.fold_left fs ~f:tr_fun_head ~init:([], env.venv) in
  let assert_fun_body (level', params, result) fun_dec =
    Trace.SemanticAnalysis.assert_fun_body fun_dec result;
    (* Here, we build another [venv''] to be used for body processing
       it should have all the arguments in it *)
    let add_param e ({ name; escapes; _ }, ty) =
      let access = Tr.alloc_local ~level:env.level ~escapes:!escapes in
      Trace.Translation.alloc_local access;
      let entry = VarEntry { ty; access } in
      ST.bind_var e name entry
    in
    let venv'' = List.fold_left params ~init:venv' ~f:add_param in
    let env' = { env with level = level'; venv = venv'' } in
    let { body; _ } = fun_dec.L.value in
    let body_ty : expr_ty = trans_expr body ~env:env' in
    if T.(body_ty.ty <> result)
    then type_mismatch_error4
        "type of the body expression doesn't match the declared result type, "
        body result body_ty.ty;
  in
  Trace.SemanticAnalysis.trans_fun_decs fs;
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

and trans_var_dec var ~env =
  let open Syntax in
  Trace.SemanticAnalysis.trans_var_dec var;
  let { var_name; init; escapes; _ } = var.L.value in
  let { ty = init_ty; _ } = trans_expr init ~env in
  assert_init var init_ty ~env;
  (* Add a new var to the term-level env *)
  let access = Tr.alloc_local ~level:env.level ~escapes:!escapes in
  Trace.Translation.alloc_local access;
  let entry = Env.VarEntry { ty = init_ty; access } in
  let venv' = ST.bind_var env.venv var_name entry in
  { env with venv = venv' }

(* Translates AST type expression into a
   digested type description that we keep in the [tenv] *)
and trans_ty tenv typ =
  let open Syntax in
  Trace.SemanticAnalysis.trans_ty typ;
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
