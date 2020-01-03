open Core_kernel
open Err

module T = Type
module L = Location
module U = Unique
module S = Symbol
module ST = Symbol_table

module Ctx = struct
  type t = {
    (* Type-level environemnt *)
    tenv : Env.tenv;
    (* Term-level environment *)
    venv : Env.venv;
    (* AST traversal path *)
    path : (Syntax.expr L.t) list;
    (* Loop marker *)
    loop : S.t option;
  }

  let mk () = {
    tenv = Env.base_tenv;
    venv = Env.base_venv;
    path = [];
    loop = None;
  }

  let enter_loop ctx name =
    { ctx with loop = Some (S.mk_unique name) }
end

type expr_ty =
  { expr : Translate.t;
    ty : T.t;
  }

let ret ty = { expr = (); ty }
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
  ignore @@ trans_expr (L.dummy expr) ~ctx:(Ctx.mk ())

and trans_expr expr ~ctx =
  let open Syntax in
  let open Ctx in

  let rec assert_ty t expr ~ctx =
    let { ty; _ } = tr_expr expr ~ctx in
    if T.(~!ty <> ~!t)
    then type_mismatch_error3 expr t ty

  and assert_int expr ~ctx = assert_ty T.Int expr ~ctx
  and assert_unit expr ~ctx = assert_ty T.Unit expr ~ctx

  and tr_expr expr ~ctx =
    (* Push the current [expr] to the [path] stack *)
    let ctx = { ctx with path = expr :: ctx.path } in
    match expr.L.value with
    | Var var -> tr_var var ~ctx
    | Nil _ -> ret_nil
    | Int _ -> ret_int
    | String _ -> ret_string
    | Call (f, args) -> tr_call f args ~ctx
    | Op (l, op, r) -> tr_op expr l r op.L.value ~ctx
    | Record (name, fields) -> tr_record name fields ~ctx
    | Seq [] -> ret_unit (* in our grammar unit is empty seq *)
    | Seq exprs -> tr_seq exprs ~ctx
    | Assign (var, expr) -> tr_assign var expr ~ctx
    | If (cond, t, f) -> tr_cond cond t f ~ctx
    | While (cond, body) -> tr_while cond body ~ctx
    | For (var, lo, hi, body, _) -> tr_for var lo hi body ~ctx
    | Break br -> tr_break br ~ctx
    | Let (decs, body) -> tr_let decs body ~ctx
    | Array (ty, size, init) -> tr_array ty size init ~ctx

  and tr_break br ~ctx =
    (* List.iteri ctx.path ~f:(fun idx node ->
     *     let expr_str = Syntax.show_expr node.L.value in
     *     print_endline @@ sprintf "\n[%d]:\n\t%s\n" idx expr_str); *)

    match ctx.loop with
    | Some _ ->
      (* TODO: Trace breaking the loop *)
      ret_unit
    | None ->
      syntax_error br "unexpected break statement"

  and tr_call f args ~ctx =
    match ST.look_fun ctx.venv f with
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
      List.iter2_exn formals args ~f:(assert_ty ~ctx);
      T.(ret ~!result)

  (* In our language binary operators work only with
     integer operands, except for (=) and (<>) *)
  and tr_op expr l r ~ctx = function
    | Syntax.Eq | Syntax.Neq ->
      assert_comparison expr l r ~ctx
    | _ ->
      assert_op l r ~ctx

  and assert_comparison expr l r ~ctx =
    let { ty = ty_l; _ } = tr_expr l ~ctx in
    let { ty = ty_r; _ } = tr_expr r ~ctx in
    if T.(~!ty_l <> ~!ty_r)
    then type_mismatch_error3 expr ty_l ty_r;
    ret ty_l

  and assert_op l r ~ctx =
    assert_int l ~ctx;
    assert_int r ~ctx;
    ret_int

  and tr_assign var expr ~ctx =
    let { ty = var_ty; _ } = tr_var var ~ctx in
    let { ty = expr_ty; _ } = tr_expr expr ~ctx in
    if T.(var_ty = expr_ty)
    then ret var_ty
    else type_error expr @@ sprintf
        "invalid assigment of type \"%s\" to a variable of type \"%s\""
        (T.to_string expr_ty) (T.to_string var_ty)

  (* Type of a sequence is a type of its last expression,
     but we need to check all previous expressions too *)
  and tr_seq exprs ~ctx =
    List.fold_left
      ~f:(fun _ expr -> tr_expr expr ~ctx)
      ~init:ret_unit
      exprs

  and tr_cond cond t f ~ctx =
    assert_int cond ~ctx;
    let { ty = t_ty; _ } = tr_expr t ~ctx in
    match f with
    | None ->
      ret t_ty
    | Some f ->
      (* If there is a false-branch then we should
         check if types of both branches match *)
      let { ty = f_ty; _ } = tr_expr f ~ctx in
      if T.(t_ty = f_ty)
      then ret t_ty
      else type_error expr @@ sprintf
          "different types of branch expressions: \"%s\" and \"%s\""
          (T.to_string t_ty) (T.to_string f_ty)

  and tr_while cond body ~ctx =
    assert_int cond ~ctx;
    assert_unit body ~ctx:(Ctx.enter_loop ctx "while");
    ret_unit

  and tr_for var lo hi body ~ctx =
    assert_int lo ~ctx;
    assert_int hi ~ctx;
    (* Add iterator var to the term-level env  *)
    let entry = Env.VarEntry T.Int in
    let venv' = ST.bind_var ctx.venv var entry in
    let ctx = Ctx.enter_loop { ctx with venv = venv' } "for" in
    assert_unit body ~ctx;
    ret_unit

  (* In Tiger declarations appear only in a "let" expression,
     the let expression modifies both:
     type-level (tenv) and term-level (venv) environments *)
  and tr_let decs body ~ctx =
    (* Update env's according to declarations *)
    let ctx' = trans_decs decs ~ctx in
    (* Then translate the body expression using
       the new augmented environments *)
    trans_expr body ~ctx:ctx'
  (* Then the new environments are discarded *)

  and tr_record_field rec_typ tfields (name, expr) ~ctx =
    (* Find a type of the field with [name] *)
    match List.Assoc.find tfields ~equal:S.equal name.L.value with
    | Some ty_field ->
      let { ty = ty_expr; _ } = tr_expr expr ~ctx in
      if T.(ty_field @<> ty_expr)
      then type_mismatch_error3 expr ty_field ty_expr
    | None ->
      missing_field_error rec_typ name

  and tr_record ty_name vfields ~ctx =
    let open T in
    (* Get the record type definition *)
    let rec_typ = ST.look_typ ctx.tenv ty_name in
    match ~!rec_typ with
    | T.Record (tfields, _) ->
      (* We want to check each field of the variable
         against the corresponding record type definition *)
      List.iter ~f:(tr_record_field rec_typ tfields ~ctx) vfields;
      ret rec_typ
    | _ ->
      type_error ty_name @@ sprintf
        "\"%s\" is not a record" (T.to_string rec_typ)

  and tr_array typ size init ~ctx =
    let open T in
    assert_int size ~ctx;
    let { ty = init_tn; _ } = tr_expr init ~ctx in
    (* Find the type of this particular array *)
    let arr_ty = ST.look_typ ctx.tenv typ in
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

  and tr_var var ~ctx =
    match var.L.value with
    | SimpleVar var ->
      tr_simple_var var ~ctx
    | FieldVar (var, field) ->
      tr_field_var var field ~ctx
    | SubscriptVar (var, sub) ->
      tr_subscript_var var sub ~ctx

  and tr_simple_var var ~ctx =
    match ST.look_var ctx.venv var with
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

  and tr_field_var var field ~ctx =
    (* TODO: Check for "nil" *)

    (* Find a type of the record variable *)
    let rec_ty = tr_var var ~ctx in
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

  and tr_subscript_var var sub ~ctx =
    let arr_ty = tr_var var ~ctx in
    match arr_ty.ty with
    | Array (tn, _) ->
      assert_int sub ~ctx;
      T.(ret ~!tn)
    | _ ->
      type_error var @@ sprintf
        "\"%s\" is not an array" (T.to_string arr_ty.ty)

  in tr_expr expr ~ctx

and trans_decs ~ctx =
  List.fold_left
    ~f:(fun ctx dec -> trans_dec dec ~ctx)
    ~init:ctx

(* Modifies and returns term-level and
   type-level environments adding the given declaration *)
and trans_dec ~ctx = function
  | TypeDec tys -> trans_tys tys ~ctx
  | FunDec fs -> trans_funs fs ~ctx
  | VarDec var -> trans_var var ~ctx

and trans_tys tys ~ctx =
  let open Syntax in
  let tr_ty_head (tns, tenv) ty_dec =
    let typ = ty_dec.L.value.type_name in
    let tn = ref None in
    (* Add a type-reference, without body *)
    let tenv' = ST.bind_typ tenv typ (T.Name (typ.L.value, tn)) in
    tn :: tns, tenv' in
  (* First, we add all the type names to the [tenv] *)
  let (tns, tenv') = List.fold_left tys ~init:([], ctx.tenv) ~f:tr_ty_head in
  let resolve_ty tn ty_dec =
    let { typ; _ } = ty_dec.L.value in
    (* Resolve the type body *)
    let t = trans_ty tenv' typ in
    tn := Some t in
  (* Resolve the (possibly mutually recursive) types *)
  List.iter2_exn (List.rev tns) tys ~f:resolve_ty;
  { ctx with tenv = tenv' }

and trans_funs fs ~ctx =
  let open Syntax in
  let open Env in
  let tr_fun_head (sigs, venv) fun_dec =
    let { fun_name; params; result_typ; _ } = fun_dec.L.value in
    (* Translate function arguments *)
    let tr_arg f = f.name, ST.look_typ ctx.tenv f.typ in
    let args = List.map params ~f:tr_arg in
    let formals = List.map args ~f:snd in
    (* Translate the result type *)
    let result = match result_typ with
      | None -> T.Unit
      | Some t -> ST.look_typ ctx.tenv t in
    let entry = FunEntry (formals, result) in
    let venv' = ST.bind_fun venv fun_name entry in
    (args, result) :: sigs, venv' in
  (* First, we add all the function entries to the [venv] *)
  let (sigs, venv') = List.fold_left fs ~f:tr_fun_head ~init:([], ctx.venv) in
  let assert_fun_body (args, result) fun_dec =
    let { body; _ } = fun_dec.L.value in
    (* Here, we build another [venv''] to be used for body processing
       it should have all the arguments in it *)
    let add_var e (name, t) = ST.bind_var e name (VarEntry t) in
    let venv'' = List.fold_left args ~init:venv' ~f:add_var in
    let ctx' = { ctx with venv = venv'' } in
    let { ty = body_ty; _ } = trans_expr body ~ctx:ctx' in
    if T.(body_ty <> result)
    then type_mismatch_error4
        "type of the body expression doesn't match the declared result type, "
        body result body_ty; in
  (* Now, lets check the bodies *)
  List.iter2_exn (List.rev sigs) fs ~f:assert_fun_body;
  { ctx with venv = venv' }

and assert_init var init_ty ~ctx =
  let open Syntax in
  let open Ctx in
  let { var_typ; init; _ } = var.L.value in
  (* Lets see if the variable is annotated *)
  match var_typ with
  | None -> ()
  | Some ann_ty ->
    (* Check if the init expression has the
       same type as the variable annotation *)
    let var_ty = ST.look_typ ctx.tenv ann_ty in
    if T.(var_ty @<> init_ty)
    then type_mismatch_error3 init var_ty init_ty

and trans_var var ~ctx =
  let open Syntax in
  let { var_name; init; _ } = var.L.value in
  let { ty = init_ty; _ } = trans_expr init ~ctx in
  assert_init var init_ty ~ctx;
  (* Add a new var to the term-level env *)
  let entry = Env.VarEntry init_ty in
  let venv' = ST.bind_var ctx.venv var_name entry in
  { ctx with venv = venv' }

(* Translates AST type expression into a
   digested type description that we keep in the [tenv] *)
and trans_ty tenv typ =
  let open Syntax in
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
