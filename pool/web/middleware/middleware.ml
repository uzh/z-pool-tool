module HttpUtils = Http_utils
module Message = HttpUtils.Message

module Tenant_middleware = struct
  let[@warning "-27"] tenant_db_of_request req =
    "econ-uzh" |> Pool_common.Database.Label.create |> Lwt_result.lift
  ;;

  let require_admin ~login_path_f =
    let fail_action = () |> login_path_f |> HttpUtils.redirect_to in
    let filter handler req =
      let%lwt tenant_db = tenant_db_of_request req in
      match tenant_db with
      | Error _ -> fail_action
      | Ok tenant_db ->
        let%lwt user =
          Service.User.Web.user_from_session
            ~ctx:[ "pool", tenant_db |> Pool_common.Database.Label.value ]
            req
        in
        (match user with
        | Some user ->
          let%lwt is_admin = Admin.user_is_admin tenant_db user in
          (match is_admin with
          | false -> fail_action
          | true -> handler req)
        | None -> fail_action)
    in
    Rock.Middleware.create ~name:"user.require.admin" ~filter
  ;;
end
