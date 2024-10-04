open Utils.Lwt_result.Infix

let src = Logs.Src.create "api.api_utils"

let respond_error
  ?(status = `Bad_request)
  ?(language = Pool_common.Language.En)
  error
  =
  let error = Pool_common.Utils.error_to_string language error in
  `Assoc [ "error", `String error ] |> Sihl.Web.Response.of_json ~status
;;

let respond ?(src = src) req result =
  let tags = Pool_context.Logger.Tags.req req in
  let context = Pool_context.Api.find req in
  match context with
  | Ok context ->
    result context
    >|- Pool_common.Utils.with_log_error ~src ~tags
    ||> (function
     | Ok result -> Sihl.Web.Response.of_json result
     | Error error -> respond_error error)
  | Error error ->
    respond_error ~status:`Internal_server_error error |> Lwt.return
;;
