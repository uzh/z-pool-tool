open Tyxml.Html

let note page_title info = div [ h1 [ txt page_title ]; p [ txt info ] ]

let error_page_not_found language () =
  note
    Pool_common.(Message.(NotFound Page) |> Utils.error_to_string language)
    Pool_common.(Message.PageNotFoundMessage |> Utils.to_string language)
;;

let error_page_terminatory ?(lang = Pool_common.Language.En) title info () =
  note
    (Pool_common.Utils.error_to_string lang title)
    (Pool_common.Utils.error_to_string lang info)
;;
