(** IR fragment *)
type t =
  (** Represents a "fragment" to be translated to assembly language *)
  | Proc of proc
  (** Represents a pseudo-instruction sequence for a string literal *)
  | String of Temp.label * string
[@@deriving show]

and proc = {
  (** The frame descriptor containing machine-specific
      information about local variables and parameters *)
  frame: Frame.t;
  (** The result returned from [Frame.proc_entry_exit1].
      These are "view shift" statements *)
  body: Ir.stmt;
} [@@deriving show]

(** Pretty-prints a fragment *)
val print : t -> string

(** Fragment storage interface module *)
module Store : sig
  (** Push a [proc] (fragment) *)
  val push_proc : proc -> unit

  (** Push the given [string] *)
  val push_string : string -> Ir.expr

  (** Clear fragment storage *)
  val reset : unit -> unit

  (** Get a list of fragments accumulated so far *)
  val result : unit -> t list
end
