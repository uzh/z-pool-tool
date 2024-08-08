open Tyxml.Html
open Changelog

let rec format_changes changes =
  let open Changes in
  let format_change (key, value) =
    let changes = format_changes value in
    span [ txt key; txt " : "; changes ]
  in
  let rec format_assoc_list acc = function
    | [] -> acc
    | hd :: tl ->
      let acc = acc @ [ format_change hd; br () ] in
      format_assoc_list acc tl
  in
  match changes with
  | Assoc assocs -> format_assoc_list [] assocs |> span
  | Change (before, after) ->
    let format json = span [ Yojson.Safe.pretty_to_string json |> txt ] in
    span [ format before; txt " => "; format after ]
;;

(* TODO: Make HTMX table, handler as well. Async call to get changelog? *)
let list changelogs =
  let row ({ user_uuid; changes; _ } : t) =
    [ Pool_common.Id.value user_uuid |> txt; changes |> format_changes ]
    |> CCList.map (fun value -> td [ value ])
    |> tr
  in
  changelogs |> CCList.map row |> table ~a:[ a_class [ "table"; "striped" ] ]
;;
