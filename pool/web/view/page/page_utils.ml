open Tyxml.Html

let note page_title info =
  div
    ~a:[ a_class [ "trim"; "narrow"; "safety-margin" ] ]
    [ h1 ~a:[ a_class [ "heading-1" ] ] [ txt page_title ]; p [ txt info ] ]
;;

let error_page_not_found language () =
  let open Pool_common in
  note
    (Message.(NotFound Field.Page) |> Utils.error_to_string language)
    (Message.PageNotFoundMessage |> Utils.to_string language)
;;

let error_page_terminatory ?(lang = Pool_common.Language.En) title info () =
  let open Pool_common in
  note (Utils.error_to_string lang title) (Utils.error_to_string lang info)
;;
