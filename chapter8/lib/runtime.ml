(* Contains names of various runtime
   definitions and helper functions *)

(* C compiler puts and underscore at the beginning of each label.
   This is a helper function to make sure that we follow the same convention *)
let label s = "_" ^ s

let initArray = "initArray"
let initRecord = "initRecord"
let compareStrings = "compareStrings"
let assertIndex = "assertIndex"
