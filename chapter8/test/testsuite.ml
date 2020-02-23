let prog_tests () = Suite.(load all)

let canon_tests () =
  [ ("linearize", Canon.Linearize.tests)
  ; ("basic_blocks", [])
  ; ("trace_schedule", [])
  ]

let () =
  let suite = prog_tests () @ canon_tests () in
  Alcotest.run "Tiger" suite
