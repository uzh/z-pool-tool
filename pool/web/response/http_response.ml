open Utils.Lwt_result.Infix
open Pool_common

let src = Logs.Src.create "web.handler.response"

let access_denied_note { Pool_context.language; _ } =
  let open Pool_message in
  Page.Utils.note
    (Utils.error_to_string language Error.AccessDenied)
    (Utils.error_to_string language Error.AccessDeniedMessage)
;;

let not_found_note { Pool_context.language; _ } error =
  Page.Utils.note
    (Utils.error_to_string language error)
    (Pool_message.PageNotFoundMessage |> Utils.to_string language)
;;

let internal_server_error_note error =
  (* TODO: Improve message *)
  Page.Utils.note (Utils.error_to_string Language.En error) "HERE COMES THE MESSAGE"
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
      let tenant_context = Pool_context.Tenant.find req |> Utils.get_or_failwith in
      Tenant.create context tenant_context page)
  with
  | _ -> Error.create page |> Lwt.return
;;

type http_error =
  | AccessDenied
  | BadRequest of (Rock.Request.t -> Rock.Response.t Lwt.t) * Pool_message.Error.t
  | NotFound of Pool_message.Error.t
[@@deriving variants]

let error_message = function
  | AccessDenied -> Pool_message.Error.AccessDenied
  | BadRequest (_, err) -> err
  | NotFound err -> err
;;

let add_context_error req ({ Pool_context.message; _ } as context) error =
  let open CCOption in
  let open Pool_message.Collection in
  message
  |> value ~default:empty
  |> add_error error
  |> return
  |> Pool_context.set_message req context
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
  | BadRequest (f, err) ->
    add_context_error req context [ err ] |> f ||> set_response_code `Bad_request
  | NotFound err -> not_found_note context err |> html_response `Not_found
;;

let with_log_http_result_error ~src ~tags res =
  res
  |> CCResult.map_err (fun err ->
    let (_ : Pool_message.Error.t) =
      err |> error_message |> Pool_common.Utils.with_log_error ~src ~tags
    in
    err)
;;

let handle
      ?(src = src)
      ?enable_cache
      req
      (result : Pool_context.t -> (Rock.Response.t, http_error) result Lwt.t)
  =
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
    Logs.warn ~src (fun m ->
      m ~tags "Context not found: %s" (Pool_message.Error.show err));
    internal_server_error_note err |> Lwt.return
;;
