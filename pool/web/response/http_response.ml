include Entity
open Utils.Lwt_result.Infix
open Pool_message
module Api = Http_response_api
module Htmx = Http_response_htmx
module Note = Http_response_notes

let src = Logs.Src.create "web.handler.response"
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
  | AccessDenied -> Note.access_denied_note context |> html_response `Unauthorized
  | BadRequest (handler, urlencoded, err) ->
    context
    |> set_context_error [ err ]
    |> set_flash_fetcher urlencoded
    |> Pool_context.set req
    |> handler
    ||> set_response_code `Bad_request
  | NotFound err -> Note.not_found_note context err |> html_response `Not_found
  | RenderError err ->
    Note.bad_request_error_note ~language:context.Pool_context.language err
    |> make_layout req context
    ||> Sihl.Web.Response.of_html ~status:`Bad_request
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
    Note.internal_server_error_response err |> Lwt.return
;;
