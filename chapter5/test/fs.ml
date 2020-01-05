open Core

let tig_ext fn =
  let (_, ext) = Filename.split_extension fn in
  match ext with
  | Some "tig" -> true
  | _ -> false

let tig_file name =
  Sys.is_file_exn name && tig_ext name

let keep name ~included ~excluded =
  (List.is_empty included || List.mem included name ~equal:String.equal) &&
  not (List.mem excluded name ~equal:String.equal)

let rec ls_rec ?(included = []) ?(excluded = []) path =
  let name = Filename.basename path in
  let ls () =
    path
    |> Sys.ls_dir
    |> List.concat_map ~f:(fun sub -> ls_rec ~included ~excluded (path ^/ sub))
  in
  let mk () = if keep name ~included ~excluded then [path] else [] in
  if tig_file path then mk () else ls ()
