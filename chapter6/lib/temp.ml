module S = Symbol

(* We use the word "temporary" to mean a value that
   is temporarily held in a register and the word
   "label" to mean some machine-language location whose
   exact address is yet to be determined, just like a
   label in assembly language.

   This module manages these two distinct sets of names *)

type t = int

type label = Symbol.t

let mk =
  let idx = ref (-1) in
  fun () ->
    incr idx;
    !idx

let mk_label name =
  let idx = ref (-1) in
  match name with
  | Some s ->
    S.symbol s
  | None ->
    incr idx;
    let name = string_of_int !idx in
    S.symbol name
