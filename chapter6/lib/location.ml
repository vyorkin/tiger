open Lexing

type loc = position * position

let range_string loc =
  let (pos_start, pos_end) = loc in
  let line_start = pos_start.pos_lnum in
  let line_end = pos_end.pos_lnum in
  let col_start = pos_start.pos_cnum - pos_start.pos_bol in
  let col_end = pos_end.pos_cnum - pos_end.pos_bol in
  Printf.sprintf "(%d,%d)-(%d,%d)"
    line_start col_start line_end col_end

type 'a t =
  { value: 'a;
    loc: loc [@printer fun fmt l -> Format.pp_print_string fmt (range_string l)]
  } [@@deriving show]

let mk value loc = { value; loc }

let dummy value = mk value (dummy_pos, dummy_pos)
