open Utils.Lwt_result.Infix

let respond req result =
  let context = Pool_context.find req in
  (* log errors *)
  (* let tags = Pool_context.Logger.Tags.req req in *)
  match context with
  | Ok context ->
    result context
    ||> (function
           | Ok result -> result
           | Error error ->
             let error =
               Pool_common.Utils.error_to_string Pool_common.Language.En error
             in
             `Assoc [ "error", `String error ])
    ||> Sihl.Web.Response.of_json
  | Error (_ : Pool_message.Error.t) -> failwith "Error"
;;
