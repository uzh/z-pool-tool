(* Extracted from the Sihl web framework (https://github.com/oxidizing/sihl), MIT license.
   Source:
   https://github.com/uzh/sihl/blob/6c3c4040413294155cda3a363edf0ff3c7e638b8/sihl/src/session.ml
   The unused test helpers ([to_sexp], [decode_session], [get_all_resp], [find_resp] and
   [set_value_req]) have been removed. *)

module StrMap = Map.Make (String)

let of_yojson yojson =
  let open Yojson.Safe.Util in
  let session_list =
    try Some (yojson |> to_assoc |> List.map (fun (k, v) -> k, to_string v)) with
    | _ -> None
  in
  session_list |> Option.map (fun s -> s |> List.to_seq |> StrMap.of_seq)
;;

let of_json json =
  try of_yojson (Yojson.Safe.from_string json) with
  | _ -> None
;;

let to_yojson session =
  `Assoc (session |> StrMap.to_seq |> List.of_seq |> List.map (fun (k, v) -> k, `String v))
;;

let to_json session = session |> to_yojson |> Yojson.Safe.to_string
