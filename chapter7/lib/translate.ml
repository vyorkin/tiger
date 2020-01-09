open Core_kernel

type expr = unit
[@@deriving show { with_path = false }]

(* We separate [Semant] from [Translate] module to
   avoid a huge, unweildy module that does both:
   type checking and semantic translation *)

type level = {
  parent: level option;
  frame: Frame.t
} [@@deriving show { with_path = false }]

(* the [level] part will be necessary later for calculating static links,
   when the variable is accessed from a (possibly) different level *)
type access = level * Frame.access [@@deriving show]

(* static link -- pointer to / address of the frame of
   the function statically enclosing current function *)

let outermost =
  let label = Temp.mk_label None in
  let frame = Frame.mk ~label ~formals:[] in
  { parent = None; frame }

let rec stack_frames level =
  match level.parent with
  | None -> [level.frame]
  | Some parent -> level.frame :: stack_frames parent

let stack_frames_path level =
  level |> stack_frames |> List.map ~f:Frame.id

(* in the semantic analysis phase [trans_dec] creates a
   new "nesting level" for each function by calling the [new_level],
   which in turn calls the [Frame.mk] to make a new frame.

   [Semant] module keeps this [level] (along with the
   label of the functions' machine-code entry point) in
   its [FunEntry] data structure *)

let new_level ~parent ~label ~formals =
  let formals = true :: formals in
  let frame = Frame.mk ~label ~formals in
  { parent; frame }

(* Returns formals (excluding the static link) *)
let formals level =
  (* exclude the SL *)
  Frame.formals level.frame
  |> List.tl_exn
  |> List.map ~f:(fun access -> level, access)

(* When [Semant] proceses a local variable declaration at
   some level, it calls [alloc_local] to create the
   variable in this level. The result is [access].

   Later, when the variable is used in an expression,
   [Semant] hands this [access] back to the [Translate] module in
   order to generate the machine code to access the variable *)

let alloc_local ~level ~escapes =
  let access = Frame.alloc_local level.frame ~escapes in
  level, access
