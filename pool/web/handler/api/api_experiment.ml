open Utils.Lwt_result.Infix
module ApiUtils = Http_utils.Api
module Field = Pool_message.Field

let src = Logs.Src.create "handler.api.experiment"

let show req =
  let open Experiment in
  let result { Pool_context.Api.database_label; _ } =
    ApiUtils.find_id Id.validate Field.Experiment req
    |> Lwt_result.lift
    >>= find database_label
    >|+ yojson_of_t
  in
  result |> ApiUtils.respond req
;;

module Access = struct
  open Experiment
  module Guardian = Middleware.Guardian

  let experiment_effects = Guardian.api_id_effects Id.validate Field.Experiment
  let read = experiment_effects Guard.Access.read
end
