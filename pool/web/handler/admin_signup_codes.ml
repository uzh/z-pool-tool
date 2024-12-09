let path = Http_utils.Url.Admin.Settings.signup_codes_path
let src = Logs.Src.create "handler.admin.signup_codes"
let create_layout req = General.create_tenant_layout req

let index req =
  Http_utils.Htmx.handler
    ~active_navigation:path
    ~error_path:path
    ~create_layout
    ~query:(module Signup_code)
    req
  @@ fun ({ Pool_context.database_label; _ } as context) query ->
  let tenant = Pool_context.Tenant.get_tenant_exn req in
  let%lwt codes = Signup_code.all ~query database_label in
  let open Page.Admin.SignupCodes in
  (if Http_utils.Htmx.is_hx_request req then list else index tenant.Pool_tenant.url)
    context
    codes
  |> Lwt_result.return
;;

module Access : sig
  val index : Rock.Middleware.t
end = struct
  let validate = Middleware.Guardian.validate_admin_entity ~any_id:true
  let index = validate Signup_code.Access.index
end
