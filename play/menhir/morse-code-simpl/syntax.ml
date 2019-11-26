type exp =
  | SYM of string (* symbol *)
  | SEP (* separator *)
  | EOF (* end of file *)
  [@@deriving show]
