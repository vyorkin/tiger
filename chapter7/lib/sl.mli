(** Static link -- pointer to / address of the frame of
    the function statically enclosing current function **)

(** We need to use static links to access variables
    declared at an outer level of static scope.

    For example, to access some variable [x] which is declared
    somewhere outside of the current level/scope/frame the
    generated IR code should look like:

    Mem(BinOp(Const k_n, Plus, Mem(BinOp(Const k_n-1, Plus,
    ...
    Mem(BinOp(Const k_1, Plus, Temp fp))))))

    where k_1,...,k_n-1 are the various SL offsets in nested functions,
    and k_n is the offset of our variable [x] in its own frame.

    This function follows static links (SL) between
    the [current] [Translate.level] of use and the [Translate.level] of [definition]. *)
val follow : cur:Translate.level -> def:Translate.level -> Ir.expr
