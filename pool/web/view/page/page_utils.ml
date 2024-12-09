open Tyxml.Html

let i18n_page ?(narrow = true) i18n =
  let classnames = [ "trim"; "safety-margin" ] in
  let classnames = if narrow then classnames @ [ "narrow" ] else classnames in
  div ~a:[ a_class classnames ] I18n.[ i18n |> content |> Content.value |> Unsafe.data ]
;;

let note page_title info =
  div
    ~a:[ a_class [ "trim"; "narrow"; "safety-margin" ] ]
    [ h1 ~a:[ a_class [ "heading-1" ] ] [ txt page_title ]; p [ txt info ] ]
;;

let error_page_not_found language () =
  let open Pool_common in
  note
    (Pool_message.(Error.NotFound Field.Page) |> Utils.error_to_string language)
    (Pool_message.PageNotFoundMessage |> Utils.to_string language)
;;

let error_page_terminatory ?(lang = Pool_common.Language.En) title info () =
  let open Pool_common in
  note (Utils.error_to_string lang title) (Utils.error_to_string lang info)
;;

let error request_id =
  Layout.Error.create
  @@ div
       [ h2 ~a:[ a_class [ "heading-2" ] ] [ txt "Internal Server Error" ]
       ; div
           ~a:[ a_class [ "text-gray" ] ]
           [ txt "An error has been caught while handling the request." ]
       ; p
           [ txt
               [%string
                 "Our administrators have been notified. Please note your request ID \
                  <b>%{request_id}</b> when contacting us."]
           ]
       ]
;;
