let read_variant of_yojson m =
  m |> Format.asprintf "[\"%s\"]" |> Yojson.Safe.from_string |> of_yojson
;;

let read_variant_opt of_yojson m =
  try Some (read_variant of_yojson m) with
  | _ -> None
;;

let[@warning "-8"] find_in_json_assoc_opt (json : Yojson.Safe.t) search_key =
  match json with
  | `Assoc values ->
    values
    |> CCList.find_opt CCFun.(fst %> CCString.equal search_key)
    |> CCOption.map snd
  | _ -> None
;;
