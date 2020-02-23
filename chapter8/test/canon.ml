open Core
open Alcotest

open Ch8
open Ch8.Ir

let label_a = ~|(Temp.mk_label (Some "a"))
let label_b = ~|(Temp.mk_label (Some "b"))

module Linearize = struct
  let stmt = testable pp_stmt equal_stmt

  let ex1 =
    Expr (ESeq (label_a, ESeq (label_b, ~@ ~$1)))

  let ex1_expected =
    [label_a; label_b; Expr (~@ ~$1)]

  let ex1_actual =
     Canon.linearize ex1

  let ex1 () =
    (check (list stmt)) "ESeq+ESeq" ex1_expected ex1_actual

  let tests =
    [ test_case "Linearize1" `Quick ex1
    (* ; test_case "Linearize (BinOp1)" `Quick ex2 *)
    ]
end
