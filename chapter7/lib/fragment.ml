open Core

(* Given a Tiger function definition comprising a
   [level] and an already-translated [body] expression,
   the [Translate] phase should produce a descriptor for
   the function containing this necessary information *)

(** IR fragment *)
type t =
  (** Represents a "fragment" to be translated to assembly language *)
  | Proc of proc
  (** Represents a pseude-instruction sequence for a string literal *)
  | String of Temp.label * string
[@@deriving show { with_path = false }]

and proc = {
  (** The frame descriptor containing machine-specific
      information about local variables and parameters *)
  frame: Frame.t;
  (** The result returned from [Frame.proc_entry_exit1].
      These are "view shift" statements *)
  body: Ir.stmt;
} [@@deriving show { with_path = false }]

(** Fragment storage interface module *)
module Store = struct
  let fs : (t list) ref = ref []

  let push_proc e =
    fs := Proc e :: !fs

  let push_string s =
    (* TODO: Use Hashtbl for String fragments *)
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
