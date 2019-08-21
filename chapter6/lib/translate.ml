(* We separate [Semant] from [Translate] module to
   avoid a huge, unweildy module that does both:
   type checking and semantic translation *)
type t = unit

type level = int

(* the [level] part will be necessary later for calculating static links,
   when the variable is accessed from a (possibly) different level *)
type access = level * Frame.access

type lvl_args = {
  parent: level;
  name: Temp.label;
  formals: bool list
}

let outermost = 1

(* in the semantic analysis phase [trans_dec] creates a
   new "nesting level" for each function by calling the [new_level],
   which in turn calls the [Frame.new_frame] to make a new frame.

   [Semant] module keeps this [level] (along with the
   label of the functions' machine-code entry point) in
   its [FunEntry] data structure *)
let new_level args = 1

let formals _ = []

(* When [Semant] proceses a local variable declaration at
   some level, it calls [alloc_local] to create the
   variable in this level. The result is [access].

   Later, when the variable is used in an expression,
   [Semant] hands this [access] back to the [Translate] module in
   order to generate the machine code to access the variable *)
let alloc_local l escapes = (1, Frame.InFrame 1)
