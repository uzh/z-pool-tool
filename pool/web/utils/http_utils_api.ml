open Utils.Lwt_result.Infix

let src = Logs.Src.create "api.api_utils"
let api_request_header = "Api-Request"

let is_api_request req =
  let headers = Rock.Request.(req.headers) in
  match Httpaf.Headers.get headers api_request_header with
  | Some "true" -> true
  | _ -> false
;;

let headers = Opium.Headers.of_list [ "Content-Type", "application/json" ]
let response_with_headers = Sihl.Web.Response.of_json ~headers

let find_id validate_and_encode field req =
  Sihl.Web.Router.param req @@ Pool_message.Field.show field
  |> validate_and_encode
;;

let respond_error
  ?(status = `Bad_request)
  ?(language = Pool_common.Language.En)
  error
  =
  let error = Pool_common.Utils.error_to_string language error in
  `Assoc [ "error", `String error ] |> response_with_headers ~status
;;

let respond ?(src = src) req result =
  let tags = Pool_context.Logger.Tags.req req in
  let context = Pool_context.Api.find req in
  match context with
  | Ok context ->
    result context
    >|- Pool_common.Utils.with_log_error ~src ~tags
    ||> (function
     | Ok result -> response_with_headers result
     | Error error -> respond_error error)
  | Error error ->
    respond_error ~status:`Internal_server_error error |> Lwt.return
;;
