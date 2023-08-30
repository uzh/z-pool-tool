let read_variant of_yojson m =
  m |> Format.asprintf "[\"%s\"]" |> Yojson.Safe.from_string |> of_yojson
;;

let read_variant_opt of_yojson m =
  try Some (read_variant of_yojson m) with
  | _ -> None
;;
