open Utils.Lwt_result.Infix
open Pool_message

let show_error = Pool_common.Utils.error_to_string
let default_language = Pool_common.Language.En
let src = Logs.Src.create "web.handler.response"

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

let set_response_code status response = Rock.Response.{ response with status }

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

type url_encoded = (string * string list) list

type http_error =
  | AccessDenied
  | BadRequest of (Rock.Request.t -> Rock.Response.t Lwt.t) * url_encoded option * Error.t
  | NotFound of Error.t

let access_denied = AccessDenied
let bad_request ?urlencoded f err = BadRequest (f, urlencoded, err)
let not_found err = NotFound err
let not_found_on_error res = Utils.Lwt_result.map_error not_found res

let bad_request_on_error ?urlencoded handler res =
  Utils.Lwt_result.map_error (bad_request ?urlencoded handler) res
;;

let error_message = function
  | AccessDenied -> Error.AccessDenied
  | BadRequest (_, _, err) -> err
  | NotFound err -> err
;;

let urlencoded_to_flash urlencoded =
  let open CCList in
  let values =
    urlencoded |> map (fun (m, k) -> m, k |> head_opt |> CCOption.get_or ~default:"")
  in
  fun key -> assoc_opt ~eq:CCString.equal key values
;;

let set_context_error error ({ Pool_context.message; _ } as context) =
  let open CCOption in
  let open Collection in
  message
  |> value ~default:empty
  |> add_error error
  |> return
  |> Pool_context.set_message context
;;

let set_flash_fetcher urlencoded context =
  let open CCFun.Infix in
  CCOption.map_or
    ~default:context
    (urlencoded_to_flash %> Pool_context.set_flash_fetcher context)
    urlencoded
;;

let handle_error context req =
  let html_response status page =
    page
    |> make_layout req context
    ||> Sihl.Web.Response.of_html
    ||> set_response_code status
  in
  function
  | AccessDenied -> access_denied_note context |> html_response `Unauthorized
  | BadRequest (handler, urlencoded, err) ->
    context
    |> set_context_error [ err ]
    |> set_flash_fetcher urlencoded
    |> Pool_context.set req
    |> handler
    ||> set_response_code `Bad_request
  | NotFound err -> not_found_note context err |> html_response `Not_found
;;

let with_log_http_result_error ~src ~tags =
  let open CCFun in
  tap (CCResult.map_err (error_message %> Pool_common.Utils.with_log_error ~src ~tags))
;;

let handle ?(src = src) ?enable_cache req result =
  let open CCResult in
  let context = Pool_context.find req in
  let tags = Pool_context.Logger.Tags.req req in
  match context with
  | Ok context ->
    let%lwt res = result context in
    res
    |> with_log_http_result_error ~src ~tags
    >|= Http_utils.set_no_cache_headers ?enable_cache
    >|= Lwt.return
    |> get_lazy (handle_error context req)
  | Error err ->
    Logs.warn ~src (fun m -> m ~tags "Context not found: %s" (Error.show err));
    internal_server_error_note err |> Lwt.return
;;
