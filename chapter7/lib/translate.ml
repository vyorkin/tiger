type expr = unit [@@deriving show]

(* We separate [Semant] from [Translate] module to
   avoid a huge, unweildy module that does both:
   type checking and semantic translation *)

type level = {
  parent: level option;
  frame: Frame.t
} [@@deriving show]

(* the [level] part will be necessary later for calculating static links,
   when the variable is accessed from a (possibly) different level *)
type access = level * Frame.access

(* static link -- pointer to / address of the frame of
   the function statically enclosing current function *)

let outermost =
  let label = Temp.mk_label None in
  { parent = None;
    frame = Frame.mk label [];
  }

(* in the semantic analysis phase [trans_dec] creates a
   new "nesting level" for each function by calling the [new_level],
   which in turn calls the [Frame.mk] to make a new frame.

   [Semant] module keeps this [level] (along with the
   label of the functions' machine-code entry point) in
   its [FunEntry] data structure *)

let mk parent label formals =
  (* Stdio.print_endline ("Frame " ^ Temp.show_label label ^ " <" ^ string_of_int (List.length formals) ^ ">"); *)
  let formals = true :: formals in
  let frame = Frame.mk label formals in
  { parent; frame }

(* Returns formals (excluding the static link) *)
let formals lev =
  (* exclude the SL *)
  let args = List.tl (Frame.formals lev.frame) in
  List.map (fun access -> lev, access) args

(* When [Semant] proceses a local variable declaration at
   some level, it calls [alloc_local] to create the
   variable in this level. The result is [access].

   Later, when the variable is used in an expression,
   [Semant] hands this [access] back to the [Translate] module in
   order to generate the machine code to access the variable *)

let alloc_local lev esc =
  (* Stdio.print_endline ("loc"); *)
  let access = Frame.alloc_local lev.frame esc in
  lev, access
