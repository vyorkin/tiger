open Core_kernel

module S = Symbol

(* We use the word "temporary" to mean a value that
   is temporarily held in a register and the word
   "label" to mean some machine-language location whose
   exact address is yet to be determined, just like a
   label in assembly language.

   This module manages these two distinct sets of names *)

(* We represent a "temporary" as a pair of
   integer (identifier) and an optional name,
   which is used for registers (and is helpful for tracing) *)
type t = int * string option
[@@deriving show { with_path = false }]

type label = Symbol.t
[@@deriving show { with_path = false }]

let mk_internal =
  let idx = ref (-1) in
  fun name -> incr idx; (!idx, name)

let mk () = mk_internal None
let mk_named name = mk_internal (Some name)

let mk_label =
  let idx = ref (-1) in
  fun name ->
    match name with
    | Some s ->
      S.mk s
    | None ->
      incr idx;
      !idx |> Int.to_string |> S.mk

let print_temp (id, name) =
  match name with
  | Some s -> s
  | None -> Int.to_string id

let print_label s =
  Symbol.(sprintf "%s <#%d>" s.name s.id)
