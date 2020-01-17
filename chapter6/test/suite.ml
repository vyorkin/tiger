open Core

module Path = struct
  open Filename

  let book = parent_dir_name ^/ "book/testcases"
  let main = parent_dir_name ^/ "testcases"

  let mk dir = main ^/ dir
end

let book_tests =
  (* failing / negative-case tests *)
  let failing = [
    9; 10; 11; 13; 14; 15; 17; 18;
    19; 20; 21; 22; 23; 24; 25;
    26; 28; 29; 31; 33; 32; 34;
    35; 36; 40; 43; 49;
  ] in
  let skipped = List.map failing ~f:(sprintf "test%d.tig") in
  let excluded = skipped @ ["merge.tig"; "queens.tig"] in
  Group.make "book tests" ~path:Path.book ~excluded

let book_examples =
  let included = ["merge.tig"; "queens.tig"] in
  Group.make "book examples" ~path:Path.book ~included

let break_for =
  let excluded = ["fail_hi.tig"; "fail_lo.tig"] in
  Group.make "break for" ~path:(Path.mk "break_for") ~excluded

let break_while =
  let excluded = ["fail_cond.tig"; "fail_let.tig"; "fail_outer.tig"] in
  Group.make "break while" ~path:(Path.mk "break_while") ~excluded

let translation =
  Group.make "translation" ~path:(Path.mk "translation")

let all = [
  book_tests;
  (* book_examples; *)
  break_for;
  break_while;
  translation;
]

let load =
  List.map ~f:Group.load
