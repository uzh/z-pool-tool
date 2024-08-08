open Tyxml.Html
open Changelog

let format_changes changes =
  Yojson.Safe.pretty_to_string changes |> Http_utils.add_line_breaks
;;

(* TODO: Make HTMX table, handler as well. Async call to get changelog? *)
let list changelogs =
  let row ({ user_uuid; changes; _ } : t) =
    let () =
      Logs.info (fun m -> m "%s" (Yojson.Safe.pretty_to_string changes))
    in
    [ Pool_common.Id.value user_uuid |> txt; changes |> format_changes ]
    |> CCList.map (fun value -> td [ value ])
    |> tr
  in
  changelogs |> CCList.map row |> table ~a:[ a_class [ "table"; "striped" ] ]
;;
