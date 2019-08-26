module S = Symbol
module Table = S.Table

type depth = int

type env = (depth * bool ref) Table.t

(* variable escapes if:
   - it is passed by reference
   - it is accessed from a nested function
   - its address is taken (using C's "&" operator) *)


(* Whenever a variable or formal-parameter declaration [a] is
   found at static function-nesting depth [d] then
   a new binding (d, ref false) is entered into the environment.

   This new environment is used in processing expressions within the
   scope of the variable.

   Then whenever this var or formal-parameter [a] is
   used at depth > d (which means that it escapes), then
   our "escape ref" is set to [true] in the environment *)

let rec traverse_prog expr =
  ()
and traverse_var env d var =
  ()
and traverse_expr env d expr =
  ()
and traverse_decs env d exprs =
  ()

(* This phase must occur before semantic analysis begins,
   since [Semant] module needs to know whether a variable
   escapes immediately upon seeing that var for the first time *)
