module ApiUtils = Http_utils.Api
module Field = Pool_message.Field
open Utils.Lwt_result.Infix

let src = Logs.Src.create "handler.api.v1.session"

let show req =
  let open Session in
  let result { Pool_context.Api.database_label; _ } =
    ApiUtils.find_id Id.validate Field.Session req
    |> Lwt_result.lift
    >>= find database_label
    >|+ yojson_of_t
  in
  result |> ApiUtils.respond ~src req
;;

module Access = struct
  open Session
  module Guardian = Middleware.Guardian

  let session_effects = Guardian.api_id_effects Id.validate Field.Session
  let read = session_effects Guard.Access.read
end
