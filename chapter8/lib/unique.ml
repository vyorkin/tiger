(* see https://github.com/janestreet/base/blob/master/src/ppx_compare_lib.ml#L26 *)
open Ppx_compare_lib.Builtin

type t = int [@@deriving compare, equal, show]

let mk =
  let n = ref (-1) in
  fun () -> incr n; !n

let to_string =
  Printf.sprintf "#%d"
