open Tyxml.Html

(* TODO[timhub]: Which layout? and how to deal with context? *)
let note language page_title info message =
  let html = div [ h1 [ txt page_title ]; p [ txt info ] ] in
  Page_layout.Tenant.create_layout html message language
;;

let error_page_not_found language () =
  note
    language
    Pool_common.(Message.(NotFound Page) |> Utils.error_to_string Language.En)
    Pool_common.(Message.PageNotFoundMessage |> Utils.to_string Language.En)
    None
;;

let error_page_terminatory ?(lang = Pool_common.Language.En) title info () =
  note
    lang
    (Pool_common.Utils.error_to_string lang title)
    (Pool_common.Utils.error_to_string lang info)
    None
;;
