open Core_kernel

(* Given a Tiger function definition comprising a
   [level] and an already-translated [body] expression,
   the [Translate] phase should produce a descriptor for
   the function containing this necessary information *)

type t =
  | Proc of proc
  | String of Temp.label * string
[@@deriving show { with_path = false }]

and proc = {
  frame: Frame.t;
  body: Ir.stmt;
} [@@deriving show { with_path = false }]

let print = function
  | Proc { frame; body; } ->
    sprintf "[PROC]:\n--FRAME--\n%s\n--BODY--\n%s"
      (Frame.Printer.print_frame frame)
      (Ir_printer.print_stmt body)
  | String (l, s) ->
    sprintf "[STRING]: \"%s\" (%s)"
      s (Temp.print_label l)

module Store = struct
  let fs : (t list) ref = ref []

  let push_proc e =
    fs := Proc e :: !fs

  let push_string s =
    (* TODO: Use [Hashtbl] for [string] fragments *)
    let matches = function
      | Proc _ -> false
      | String (_, s') -> String.equal s s'
    in
    let label =
      match List.find !fs ~f:matches with
      | Some (String (l, _)) -> l
      | _ ->
        let l = Temp.mk_label None in
        fs := String (l, s) :: !fs;
        l
    in Ir.(~:label)

  let reset () =
    fs := []

  let result () =
    !fs
end
