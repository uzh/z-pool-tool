open Pool_message

let show_error = Pool_common.Utils.error_to_string
let default_language = Pool_common.Language.En

let access_denied_note { Pool_context.language; _ } =
  Page.Utils.note
    (show_error language Error.AccessDenied)
    (show_error language Error.AccessDeniedMessage)
;;

let not_found_note { Pool_context.language; _ } error =
  Page.Utils.note
    (show_error language error)
    (PageNotFoundMessage |> Pool_common.Utils.to_string language)
;;

let bad_request_error_note ?(language = default_language) error =
  Page.Utils.note (show_error language Error.BadRequest) (show_error language error)
;;

let htmx_onpage_error_note language error =
  let open Pool_message in
  Page.Utils.note
    (show_error language Error.InternalServerError)
    (show_error language error)
;;

let internal_server_error_response error =
  Page.Utils.note
    (show_error default_language Error.InternalServerError)
    (show_error default_language error)
  |> Layout.Error.create
  |> Sihl.Web.Response.of_html ~status:`Internal_server_error
;;
