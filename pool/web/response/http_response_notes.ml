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

let internal_server_error_note error =
  Page.Utils.note
    (show_error default_language Error.InternalServerError)
    (show_error default_language error)
  |> Layout.Error.create
  |> Sihl.Web.Response.of_html ~status:`Internal_server_error
;;

let htmx_onpage_error language error =
  let open Pool_message in
  let show_error = Pool_common.Utils.error_to_string in
  Page.Utils.note
    (show_error language Error.InternalServerError)
    (show_error language error)
;;
