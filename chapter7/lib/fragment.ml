(** Given a Tiger function definition comprising a
    [level] and an already-translated [body] expression,
    the [Translate] phase should produce a descriptor for
    the function containing this necessary information *)
type t =
  | Proc of proc
  | String of Temp.label * string
and proc = {
  (** The frame descriptor containing machine-specific
      information about local variables and parameters *)
  frame: Frame.t;
  (** The result returned from [Translate.proc_entry_exit1] *)
  body: Ir.stmt;
}
