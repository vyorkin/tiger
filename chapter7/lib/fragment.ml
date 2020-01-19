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
  [@@deriving show]

and proc = {
  (** The frame descriptor containing machine-specific
      information about local variables and parameters *)
  frame: Frame.t;
  (** The result returned from [Frame.proc_entry_exit1].
      These are "view shift" statements *)
  body: Ir.stmt;
}
