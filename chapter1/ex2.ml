open Base
open Stdio
open Straightline

let eval x op y =
  match op with
  | Plus  -> x + y
  | Minus -> x - y
  | Times -> x * y
  | Div   -> x / y

let rec interp_stm stm env =
  let print env0 exp =
    let (v, env1) = interp_exp exp env0 in
    print_endline (Int.to_string v);
    env1
  in
  match stm with
  | Print exps ->
    List.fold_left ~init:env ~f:print exps
  | Assign (key, exp) ->
    let (data, env1) = interp_exp exp env in
    Map.set env1 ~key ~data
  | Compound (stm1, stm2) ->
    let env1 = interp_stm stm1 env in
    interp_stm stm2 env1

and interp_exp exp env =
  match exp with
  | Id k ->
    let v = Map.find_exn env k in (v, env)
  | Num x -> (x, env)
  | Op (exp1, op, exp2) ->
    let (x, env1) = interp_exp exp1 env in
    let (y, env2) = interp_exp exp2 env1 in
    let r = eval x op y in
    (r, env2)
  | Eseq (stm, exp) ->
    let env1 = interp_stm stm env in
    interp_exp exp env1
