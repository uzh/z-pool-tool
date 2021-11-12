open Tyxml.Html

let note page_title info message =
  let html = div [ h1 [ txt page_title ]; p [ txt info ] ] in
  Page_layout.create html message ()
;;

let error_page_not_found () =
  Pool_common.(
    Message.(
      note
        (NotFound Page |> Utils.error_to_string Language.En)
        (PageNotFoundMessage |> Utils.to_string Language.En)
        None))
;;
