open Base
open Straightline

let rec maxargs stm =
  let maxargs_exp = function
    | Eseq (stm, _) -> maxargs stm
    | _ -> 0
  in match stm with
  | Print exps ->
    let len = List.length exps in
    let exp_lens = List.map ~f:maxargs_exp exps in
    let max_exp_len = List.max_elt ~compare:Int.compare exp_lens in
    Int.max len (Option.value ~default:0 max_exp_len)
  | Compound (exp1, exp2) ->
    let l1 = maxargs exp1 in
    let l2 = maxargs exp2 in
    Int.max l1 l2
  | Assign (_, exp) ->
    maxargs_exp exp
