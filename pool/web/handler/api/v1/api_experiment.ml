module ApiUtils = Http_utils.Api
module Field = Pool_message.Field
open Utils.Lwt_result.Infix

let src = Logs.Src.create "handler.api.v1.experiment"

let index req =
  let open Experiment in
  let result { Pool_context.Api.database_label; _ } actor query =
    find_all
      ~actor
      ~query
      ~permission:Guard.Access.index_permission
      database_label
    |> Lwt_result.ok
  in
  result
  |> ApiUtils.index_handler ~query:(module Experiment) ~yojson_of_t ~src req
;;

let show req =
  let open Experiment in
  let result { Pool_context.Api.database_label; _ } =
    ApiUtils.find_id Id.validate Field.Experiment req
    |> Lwt_result.lift
    >>= find database_label
    >|+ yojson_of_t
  in
  result |> ApiUtils.respond ~src req
;;

module Access = struct
  open Experiment
  module Guardian = Middleware.Guardian

  let experiment_effects = Guardian.api_id_effects Id.validate Field.Experiment
  let index = Guardian.api_validate_admin_entity ~any_id:true Guard.Access.index
  let read = experiment_effects Guard.Access.read
end
