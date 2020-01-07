let () =
  let suite = Suite.(load all) in
  Alcotest.run "Tiger" suite
