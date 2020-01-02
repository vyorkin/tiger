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
    9; 10; 11; 13; 14; 17; 18;
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

let break_for = Group.make "break for" ~path:(Path.mk "break_for")
let break_while = Group.make "break while" ~path:(Path.mk "break_while")

let all = [
  book_tests;
  (* book_examples; *)
  break_for;
  break_while;
]

let load =
  List.map ~f:Group.load
