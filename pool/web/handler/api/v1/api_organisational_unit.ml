module Response = Http_response.Api
open Utils.Lwt_result.Infix

let src = Logs.Src.create "handler.api.v1.organisational_unit"

let index req =
  let result { Pool_context.Api.database_label; _ } =
    Database.connection_ctx database_label @@ fun db_ctx ->
    Organisational_unit.all db_ctx ()
    ||> CCList.map Organisational_unit.yojson_of_t
    ||> (fun json -> `List json)
    |> Lwt_result.ok
  in
  result |> Response.handle ~src req
;;

module Access = struct
  open Organisational_unit
  module Guardian = Middleware.Guardian

  let index = Guardian.api_validate_admin_entity ~any_id:true Guard.Access.index
end
