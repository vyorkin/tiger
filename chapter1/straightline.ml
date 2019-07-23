(* straight-line *)

type id = string

type binop = Plus | Minus | Times | Div

type stm = Compound of stm * stm
         | Assign of id * exp
         | Print of exp list

 and exp = Id of id
         | Num of int
         | Op of exp * binop * exp
         | Eseq of stm * exp

(* a := 5 * 3; b := (print(a, a - 1), a * 10); print(b) *)

let prog =
  Compound(
    Assign("a", Op(Num 5, Plus, Num 3)),
    Compound(
      Assign(
        "b",
        Eseq(
          Print [Id "a"; Op(Id "a", Minus, Num 1)],
          Op(Num 10, Times, Id "a")
        )
      ),
      Print [Id "b"]
    )
  )
