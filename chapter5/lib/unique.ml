open Printf

type t = int [@@deriving eq]

let mk =
  let n = ref (-1) in
  fun () ->
    incr n;
    !n

let to_string =
  sprintf "#%d"
