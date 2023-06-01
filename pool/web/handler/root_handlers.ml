module Login = Root_login
module Profile = Root_user_profile
module Settings = Root_settings
module Tenant = Root_tenant
module Users = Root_users

let forward_to_entrypoint req =
  let open Utils.Lwt_result.Infix in
  Service.User.Web.user_from_session ~ctx:Pool_database.(to_ctx root) req
  >|> function
  | Some _ -> Http_utils.redirect_to "/root/tenants"
  | None -> Http_utils.redirect_to "/root/login"
;;
