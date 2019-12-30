open Core
open Lexing
open OUnit2
open Ch7
open Ch7.Lexer

module Sys = Core.Sys
module Filename = Core.Filename

let skipped = [
  "test9.tig";
  "test10.tig";
  "test11.tig";
  "test13.tig";
  "test14.tig";
  "test17.tig";
  "test18.tig";
  "test19.tig";
  "test20.tig";
  "test21.tig";
  "test22.tig";
  "test23.tig";
  "test24.tig";
  "test25.tig";
  "test26.tig";
  "test28.tig";
  "test29.tig";
  "test31.tig";
  "test33.tig";
  "test32.tig";
  "test34.tig";
  "test35.tig";
  "test36.tig";
  "test40.tig";
  "test43.tig";
  "test49.tig";
  "merge.tig";
  "queens.tig"
]

let is_tig_ext filename =
  let (_, ext) = Filename.split_extension filename in
  match ext with
  | Some "tig" -> true
  | _ -> false

let is_tig_file f =
  Sys.is_file_exn ~follow_symlinks:true f &&
  is_tig_ext f

let rec ls_rec dir =
  if is_tig_file dir
  then
    if not(List.mem skipped (Filename.basename dir) ~equal:(=)) then
      [dir]
    else
      []
  else
    dir
    |> Sys.ls_dir
    |> List.concat_map
      ~f:(fun sub -> ls_rec (Filename.concat dir sub))

let position lexbuf =
  let pos = lexbuf.lex_curr_p in
  let col = pos.pos_cnum - pos.pos_bol + 1 in
  Printf.sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum col

let parse_with_error lexbuf =
  try
    let expr = Parser.main Lexer.read lexbuf in
    Semant.trans_prog expr;
    assert_bool "Ok" true
  with
  | LexingError msg ->
    assert_failure (position lexbuf ^ " : " ^ msg)
  | Parser.Error ->
    assert_failure ("Syntax error: " ^ position lexbuf)
  | Error.Error (err, loc, msg) ->
    assert_failure (Error.to_string err loc msg)

let parse filename ch =
  let lexbuf = Lexing.from_channel ch in
  lexbuf.lex_curr_p <- {
    lexbuf.lex_curr_p with pos_fname = Filename.basename filename
  };
  parse_with_error lexbuf

let run_parser filename _ =
  In_channel.with_file filename ~f:(parse filename)

let suite =
  "tiger programs" >:::
  let tests_dir = "../../../book" ^ Filename.dir_sep ^ "testcases" in
  let tests_path = Filename.(concat parent_dir_name tests_dir) in
  let tig_files = ls_rec tests_path in
  (List.map ~f:
     (fun filename ->
        Filename.basename filename >:: run_parser filename)
     tig_files)

let () =
  run_test_tt_main suite
