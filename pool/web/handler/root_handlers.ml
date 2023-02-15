module Login = Root_login
module Settings = Root_settings
module Tenant = Root_tenant
module Users = Root_users

let forward_to_entrypoint entrypoint _ = Http_utils.redirect_to entrypoint
