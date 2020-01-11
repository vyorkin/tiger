open Ir

module F = Frame

(* One thing to notice:

   We always know that SL is the first in the
   list of formals of a stack frame, so it always
   looks like [InFrame 0]. This means that we can simplify our
   formula to the following degenerated case:

   Mem(k_n <+> Mem(K_n-1 <+> ... <+> Mem(K_1 + FP)))
   where
   K_n = K_n-1 = ... K_1 = 0

   So the resulting expression becomes just:

   Mem(Mem(...Mem(FP)))

   But let's implement a general-form of it *)

let rec follow ~cur ~def =
  let open Translate in
  if F.(cur.frame = def.frame)
  then
    (* Variable is defined in the current scope/level *)
    Temp F.fp
  else
    (* Variable is defined in an outer scope/level *)
    match F.formals cur.frame with
    | sl :: _ ->
      (match cur.parent with
       | None ->
         failwith "Nested level without parent"
       | Some parent ->
         let addr = follow ~cur:parent ~def in
         F.expr sl ~addr
      )
    | [] ->
      failwith "No static link in formals"
