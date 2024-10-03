open Utils.Lwt_result.Infix

let json_error ?(language = Pool_common.Language.En) error =
  let error = Pool_common.Utils.error_to_string language error in
  `Assoc [ "error", `String error ]
;;

let respond req result =
  let context = Pool_context.find req in
  (* log errors *)
  (* let tags = Pool_context.Logger.Tags.req req in *)
  match context with
  | Ok context ->
    result context
    ||> (function
           | Ok result -> result
           | Error error -> json_error error)
    ||> Sihl.Web.Response.of_json
  | Error error -> json_error error |> Sihl.Web.Response.of_json |> Lwt.return
;;
