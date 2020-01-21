open Core

type t =
  { name: string [@main];
    path: string;
    excluded: string list;
    included: string list;
  } [@@deriving make]

let load group =
  let { excluded; included; _ } = group in
  let tests =
    group.path
    |> Fs.ls_rec ~included ~excluded
    |> List.map ~f:Test.mk in
  group.name, tests
