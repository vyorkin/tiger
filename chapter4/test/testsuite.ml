open Core
open Lexing
open OUnit2
open Ch4
open Ch4.Lexer

module Sys = Core.Sys
module Filename = Core.Filename

let skipped =
  ["test16.tig"; "test17.tig"; "test19.tig";
   "test20.tig"; "test25.tig";
   "test45.tig"; "test49.tig"]

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
    Parser.main Lexer.read lexbuf |> ignore;
    assert_bool "Ok" true
  with
  | SyntaxError msg ->
    assert_failure (position lexbuf ^ " : " ^ msg)
  | Parser.Error ->
    assert_failure ("Syntax error: " ^ position lexbuf)

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
