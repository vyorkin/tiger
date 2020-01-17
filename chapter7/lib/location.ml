open Lexing

type loc = position * position

let range_string loc =
  let (pos_start, pos_end) = loc in
  let line_start = pos_start.pos_lnum in
  let line_end = pos_end.pos_lnum in
  let col_start = pos_start.pos_cnum - pos_start.pos_bol in
  let col_end = pos_end.pos_cnum - pos_end.pos_bol in
  match (line_start, col_start, line_end, col_end) with
  | (0, -1, 0, -1) -> "(?, ?)-(?, ?)"
  | (ls, cs, le, ce) -> Printf.sprintf "(%d,%d)-(%d,%d)" ls cs le ce

type 'a t =
  { value: 'a;
    loc: loc [@printer fun fmt l -> Format.pp_print_string fmt (range_string l)]
  } [@@deriving show { with_path = false }]

let mk value loc = { value; loc }

let dummy value =
  mk value (dummy_pos, dummy_pos)

let (~?) = dummy

let line { loc = (pos_start, _); _ } =
  pos_start.pos_lnum
