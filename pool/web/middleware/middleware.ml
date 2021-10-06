module Tenant_middleware = struct
  let[@warning "-27"] tenant_db_of_request req =
    "econ-uzh" |> Pool_common.Database.Label.create |> Lwt_result.lift
  ;;
end
