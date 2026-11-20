open Utils.Lwt_result.Infix
module Field = Pool_message.Field
module HttpUtils = Http_utils
module Response = Http_response
module Command = Cqrs_command.Signup_code_command
module View = Page.Admin.SignupCodes

let field = Field.SignUpCode
let path = HttpUtils.Url.Admin.Settings.signup_codes_path
let active_navigation = path ()
let src = Logs.Src.create "handler.admin.signup_codes"
let create_layout req = General.create_tenant_layout req
let id = HttpUtils.find_id Signup_code.Id.of_string field

let index req =
  Response.Htmx.index_handler
    ~active_navigation
    ~create_layout
    ~query:(module Signup_code)
    req
  @@ fun context query ->
  let%lwt codes = Pool_context.connection context @@ Signup_code.all ~query in
  let tenant = Pool_context.Tenant.get_tenant_exn req in
  (if HttpUtils.Htmx.is_hx_request req then View.list else View.index)
    context
    tenant.Pool_tenant.url
    codes
  |> Lwt_result.return
;;

let new_form req =
  let result context =
    View.new_form context
    |> create_layout req ~active_navigation context
    >|+ Sihl.Web.Response.of_html
    |> Response.bad_request_render_error context
  in
  Response.handle ~src req result
;;

let edit req =
  let result context =
    let* signup_code =
      Pool_context.connection context
      @@ fun db_ctx -> Signup_code.find db_ctx (id req) >|- Response.not_found
    in
    let tenant = Pool_context.Tenant.get_tenant_exn req in
    View.edit context tenant.Pool_tenant.url signup_code
    |> create_layout req ~active_navigation context
    >|+ Sihl.Web.Response.of_html
    |> Response.bad_request_render_error context
  in
  Response.handle ~src req result
;;

let create req =
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
  in
  let result ({ Pool_context.user; _ } as context) =
    Response.bad_request_on_error ~urlencoded new_form
    @@
    let tags = Pool_context.Logger.Tags.req req in
    Pool_context.connection context
    @@ fun db_ctx ->
    let id = Signup_code.Id.create () in
    let* command = Command.Create.decode urlencoded |> Lwt_result.lift in
    let%lwt existing = Signup_code.find_by_code db_ctx command.Command.code in
    let* events = Command.Create.handle ~tags ~id existing command |> Lwt_result.lift in
    let%lwt () = Pool_event.handle_events ~tags db_ctx user events in
    HttpUtils.redirect_to_with_actions
      (path ~id ())
      [ HttpUtils.Message.set ~success:[ Pool_message.Success.Created field ] ]
    |> Lwt_result.ok
  in
  Response.handle ~src req result
;;

let update req =
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
  in
  let id = id req in
  let result ({ Pool_context.user; _ } as context) =
    Pool_context.connection context
    @@ fun db_ctx ->
    let* signup_code = Signup_code.find db_ctx id >|- Response.not_found in
    Response.bad_request_on_error ~urlencoded edit
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let* events =
      Command.Update.(
        urlencoded |> decode |> Lwt_result.lift >== handle ~tags signup_code)
    in
    let%lwt () = Pool_event.handle_events ~tags db_ctx user events in
    HttpUtils.redirect_to_with_actions
      (path ~id ())
      [ HttpUtils.Message.set ~success:[ Pool_message.Success.Updated field ] ]
    |> Lwt_result.ok
  in
  Response.handle ~src req result
;;

module Access : module type of Helpers.Access = struct
  include Helpers.Access
  module Guardian = Middleware.Guardian

  let signup_code_effects = Guardian.id_effects Signup_code.Id.validate field
  let index = Guardian.validate_admin_entity ~any_id:true Signup_code.Access.index
  let create = Guardian.validate_admin_entity Command.Create.effects
  let update = signup_code_effects Command.Update.effects
end
