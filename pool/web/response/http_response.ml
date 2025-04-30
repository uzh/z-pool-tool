include Entity
open Utils.Lwt_result.Infix
open Pool_message
module Api = Http_response_api
module Htmx = Http_response_htmx
module Page = Http_response_page

let src = Logs.Src.create "web.handler.response"
let set_response_code status response = Rock.Response.{ response with status }

(** TODO: Should values like password be filtered here or simply ignored in the UI? See admin create form
*)
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
    |> Page.make_layout req context
    ||> Sihl.Web.Response.of_html
    ||> set_response_code status
  in
  function
  | AccessDenied -> Page.access_denied_note context |> html_response `Unauthorized
  | BadRequest (handler, urlencoded, err) ->
    context
    |> set_context_error [ err ]
    |> set_flash_fetcher urlencoded
    |> Pool_context.set req
    |> handler
    ||> set_response_code `Bad_request
  | NotFound err -> Page.not_found_note context err |> html_response `Not_found
;;

let with_log_http_result_error ~src ~tags =
  let open CCFun in
  tap (CCResult.map_err (error_message %> Pool_common.Utils.with_log_error ~src ~tags))
;;

let bad_request_render_error context run =
  run
  >|- fun err ->
  let fallback req = Page.generic_error_response req context err in
  bad_request fallback err
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
    Page.internal_server_error_response err |> Lwt.return
;;
