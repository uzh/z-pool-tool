open Pool_message
open Utils.Lwt_result.Infix
open Entity

let show_error = Pool_common.Utils.error_to_string

let make_layout req context page =
  let open Layout in
  let is_root = Http_utils.is_req_from_root_host req in
  try
    if is_root
    then Root.create context page
    else (
      let tenant_context =
        Pool_context.Tenant.find req |> Pool_common.Utils.get_or_failwith
      in
      Tenant.create context tenant_context page)
  with
  | _ -> Error.create page |> Lwt.return
;;

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

let internal_server_error_response error =
  Page.Utils.note
    (show_error default_language Error.InternalServerError)
    (show_error default_language error)
  |> Layout.Error.create
  |> Sihl.Web.Response.of_html ~status:`Internal_server_error
;;

let generic_error_response
      ?(title = Pool_message.Error.BadRequest)
      ?(status = `Bad_request)
      req
      ({ Pool_context.language; _ } as context)
      error
  =
  Page.Utils.note (show_error language title) (show_error language error)
  |> make_layout req context
  ||> Sihl.Web.Response.of_html ~status
;;
