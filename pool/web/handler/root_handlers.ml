module Login = Root_login
module Root = Root_root
module Tenant = Root_tenant

let forward_to_entrypoint entrypoint _ = Http_utils.redirect_to entrypoint
