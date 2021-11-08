open Tyxml.Html

let note page_title info message =
  let html = div [ h1 [ txt page_title ]; p [ txt info ] ] in
  Page_layout.create html message ()
;;

let error_page_not_found () =
  Pool_common.Error.(
    note (NotFound Page |> to_string) (PageNotFoundMessage |> to_string) None)
;;
