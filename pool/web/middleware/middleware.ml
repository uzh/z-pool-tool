module HttpUtils = Http_utils
module Message = HttpUtils.Message

module Tenant_middleware = struct
  let[@warning "-27"] tenant_db_of_request req =
    "econ-uzh" |> Pool_common.Database.Label.create |> Lwt_result.lift
  ;;

  let require_admin ~login_path_f =
    let fail_action actions =
      let login_path = login_path_f () in
      HttpUtils.redirect_to_with_actions login_path actions
    in
    let filter handler req =
      let%lwt tenant_db = tenant_db_of_request req in
      match tenant_db with
      | Error _ -> fail_action []
      | Ok tenant_db ->
        (* TODO [timhub]: add pool context *)
        let%lwt user = Service.User.Web.user_from_session req in
        (match user with
        | Some user ->
          let%lwt is_admin = Admin.user_is_admin tenant_db user in
          (match is_admin with
          | Error err -> fail_action [ Message.set ~error:[ err ] ]
          | Ok is_admin -> if is_admin then handler req else [] |> fail_action)
        | None -> fail_action [])
    in
    Rock.Middleware.create ~name:"user.require.admin" ~filter
  ;;
end
