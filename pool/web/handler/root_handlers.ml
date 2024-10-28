module Announcement = Root_announcements
module Login = Root_login
module Profile = Root_user_profile
module Settings = Root_settings
module Status = Root_status
module Tenant = Root_tenant
module Users = Root_users
module Version = Root_version

let forward_to_entrypoint req =
  let open Utils.Lwt_result.Infix in
  Pool_user.Web.user_from_session Database.root req
  >|> function
  | Some _ -> Http_utils.redirect_to "/root/tenants"
  | None -> Http_utils.redirect_to "/root/login"
;;
