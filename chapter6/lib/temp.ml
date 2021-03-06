open Core_kernel

module S = Symbol

(* We use the word "temporary" to mean a value that
   is temporarily held in a register and the word
   "label" to mean some machine-language location whose
   exact address is yet to be determined, just like a
   label in assembly language.

   This module manages these two distinct sets of names *)

type t = int
[@@deriving show { with_path = false }]

type label = Symbol.t
[@@deriving show { with_path = false }]

let mk =
  let idx = ref (-1) in
  fun () -> incr idx; !idx

let mk_label name =
  let idx = ref (-1) in
  match name with
  | Some s ->
    S.mk s
  | None ->
    incr idx;
    !idx |> Int.to_string |> S.mk

let print_label s =
  Symbol.(sprintf "%s <#%d>" s.name s.id)
