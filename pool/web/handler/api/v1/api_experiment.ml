module ApiUtils = Http_utils.Api
module Field = Pool_message.Field
module Response = Http_response.Api
open Utils.Lwt_result.Infix

let src = Logs.Src.create "handler.api.v1.experiment"

let index req =
  let open Experiment in
  let result { Pool_context.Api.database_label; _ } actor query =
    Database.connection_ctx database_label @@ fun db_ctx ->
    list_by_user ~query db_ctx actor |> Lwt_result.ok
  in
  result |> Response.index_handler ~query:(module Experiment) ~yojson_of_t ~src req
;;

let show req =
  let open Experiment in
  let result { Pool_context.Api.database_label; _ } =
    Database.connection_ctx database_label @@ fun db_ctx ->
    ApiUtils.find_id Id.validate Field.Experiment req
    |> Lwt_result.lift
    >>= find db_ctx
    >|+ yojson_of_t
  in
  result |> Response.handle ~src req
;;

module Access = struct
  open Experiment
  module Guardian = Middleware.Guardian

  let experiment_effects = Guardian.api_id_effects Id.validate Field.Experiment
  let index = Guardian.api_validate_admin_entity ~any_id:true Guard.Access.index
  let read = experiment_effects Guard.Access.read
end
