open Utils.Lwt_result.Infix
open Pool_message
module Command = Cqrs_command.Smtp_command
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Response = Http_response
module SmtpAuth = Email.SmtpAuth

let src = Logs.Src.create "handler.admin.settings_schedule"
let active_navigation = Page.Admin.Settings.Smtp.base_path
let boolean_fields = Field.([ DefaultSmtpServer ] |> CCList.map show)
let smtp_auth_id req = HttpUtils.find_id SmtpAuth.Id.of_string Field.Smtp req

let settings_detail_path location req =
  active_navigation location
  |> fun base ->
  match location with
  | `Tenant -> Format.asprintf "%s/%s" base (req |> smtp_auth_id |> SmtpAuth.Id.value)
  | `Root -> base
;;

let email_of_urlencoded urlencoded =
  let field = Field.EmailAddress in
  urlencoded
  |> CCList.assoc_opt ~eq:( = ) Field.(show field)
  |> CCFun.flip CCOption.bind CCList.head_opt
  |> CCOption.to_result Error.(Missing field)
  |> Lwt_result.lift
  >== Pool_user.EmailAddress.create
;;

let index req =
  let location = `Tenant in
  let active_navigation = active_navigation location in
  Response.Htmx.index_handler
    ~active_navigation
    ~query:(module SmtpAuth)
    ~create_layout:General.create_tenant_layout
    req
  @@ fun ({ Pool_context.database_label; _ } as context) query ->
  let%lwt smtp_list, query = SmtpAuth.find_by query database_label in
  let open Page.Admin.Settings.Smtp in
  (if HttpUtils.Htmx.is_hx_request req then list else index)
    context
    location
    smtp_list
    query
  |> Lwt_result.return
;;

let new_form req =
  let location = `Tenant in
  let active_navigation = active_navigation location in
  let result context =
    Response.bad_request_render_error context
    @@
    let open Page.Admin.Settings.Smtp in
    smtp_create_form context location
    |> General.create_tenant_layout req ~active_navigation context
    >|+ Sihl.Web.Response.of_html
  in
  Response.handle ~src req result
;;

let smtp_form location req =
  let result ({ Pool_context.database_label; _ } as context) =
    Response.bad_request_render_error context
    @@
    let open Page.Admin.Settings.Smtp in
    let active_navigation = active_navigation location in
    let* html =
      match location with
      | `Tenant ->
        req
        |> smtp_auth_id
        |> SmtpAuth.find database_label
        >|+ show context location
        >>= General.create_tenant_layout req ~active_navigation context
      | `Root ->
        Database.Pool.Root.label
        |> SmtpAuth.find_default
        ||> CCResult.to_opt
        ||> (function
         | Some auth -> show context location auth
         | None -> smtp_create_form context location)
        >|> General.create_root_layout ~active_navigation context
        ||> CCResult.return
    in
    html |> Sihl.Web.Response.of_html |> Lwt_result.return
  in
  Response.handle ~src req result
;;

let show = smtp_form `Tenant

let create_post location req =
  let open Command.Create in
  let tags = Pool_context.Logger.Tags.req req in
  let redirect_path = active_navigation location in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req
    ||> HttpUtils.format_request_boolean_values boolean_fields
    ||> HttpUtils.remove_empty_values
  in
  let result { Pool_context.database_label; user; _ } =
    Response.bad_request_on_error ~urlencoded new_form
    @@
    let validate_label ({ Command.label; _ } as m : Command.create) =
      SmtpAuth.find_by_label database_label label
      ||> function
      | Some _ -> Error (Error.Uniqueness Field.SmtpLabel)
      | None -> Ok m
    in
    let%lwt default_smtp = SmtpAuth.find_default_opt database_label in
    let test_smtp_config smtp_auth =
      let* email = email_of_urlencoded urlencoded in
      let* () = Email.Service.test_smtp_config database_label smtp_auth email in
      Lwt_result.return smtp_auth
    in
    let events = handle ~tags default_smtp in
    let handle = Pool_event.handle_events ~tags database_label user in
    let return_to_overview () =
      HttpUtils.redirect_to_with_actions
        redirect_path
        [ Message.set ~success:[ Success.SmtpConfigurationAdded ] ]
    in
    urlencoded
    |> decode
    |> Lwt_result.lift
    >>= validate_label
    >== smtp_of_command
    >>= test_smtp_config
    >== events
    |>> handle
    |>> return_to_overview
  in
  Response.handle ~src req result
;;

let create = create_post `Tenant

let update_base location command success_message req =
  let tags = Pool_context.Logger.Tags.req req in
  let redirect_path = settings_detail_path location req in
  let result { Pool_context.database_label; user; _ } =
    let%lwt urlencoded =
      Sihl.Web.Request.to_urlencoded req
      ||> HttpUtils.format_request_boolean_values boolean_fields
      ||> HttpUtils.remove_empty_values
    in
    let* smtp_auth =
      req |> smtp_auth_id |> SmtpAuth.find database_label >|- Response.not_found
    in
    Response.bad_request_on_error ~urlencoded (smtp_form location)
    @@
    let events (_ : SmtpAuth.t) =
      let open CCResult.Infix in
      match command with
      | `UpdateDetails ->
        let%lwt default_smtp = SmtpAuth.find_default_opt database_label in
        Command.Update.(decode urlencoded >>= handle ~tags default_smtp smtp_auth)
        |> Lwt_result.lift
      | `UpdatePassword ->
        Command.UpdatePassword.(decode urlencoded >>= handle ~tags smtp_auth)
        |> Lwt_result.lift
    in
    let handle = Pool_event.handle_events ~tags database_label user in
    let return_to_overview () =
      HttpUtils.redirect_to_with_actions
        redirect_path
        [ Message.set ~success:[ success_message ] ]
    in
    req
    |> smtp_auth_id
    |> SmtpAuth.find database_label
    >>= events
    |>> handle
    |>> return_to_overview
  in
  Response.handle ~src req result
;;

let update_password = update_base `Tenant `UpdatePassword Success.SmtpPasswordUpdated
let update = update_base `Tenant `UpdateDetails Success.SmtpDetailsUpdated

let delete_base location req =
  let tags = Pool_context.Logger.Tags.req req in
  let path = active_navigation location in
  let result { Pool_context.database_label; user; _ } =
    let* smtp =
      Sihl.Web.Router.param req (Field.show Field.Smtp)
      |> SmtpAuth.Id.of_string
      |> SmtpAuth.find database_label
      >|- Response.not_found
    in
    Response.bad_request_on_error index
    @@ (Cqrs_command.Smtp_command.Delete.handle ~tags smtp.SmtpAuth.id
        |> Lwt_result.lift
        |>> Pool_event.handle_events ~tags database_label user
        |>> fun () ->
        Http_utils.redirect_to_with_actions
          path
          [ Message.set ~success:[ Success.Deleted Field.Smtp ] ])
  in
  Response.handle ~src req result
;;

let delete = delete_base `Tenant

let validate location req =
  let open Utils.Lwt_result.Infix in
  let id = req |> smtp_auth_id in
  let redirect_path = settings_detail_path location req in
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let result { Pool_context.database_label; _ } =
    let* smtp = SmtpAuth.find_full database_label id >|- Response.not_found in
    Response.bad_request_on_error ~urlencoded (smtp_form location)
    @@
    let* email = email_of_urlencoded urlencoded in
    let redirect actions =
      Http_utils.redirect_to_with_actions redirect_path actions ||> CCResult.return
    in
    Email.Service.test_smtp_config database_label smtp email
    >|> function
    | Ok () -> redirect [ Message.set ~success:[ Success.Validated Field.Smtp ] ]
    | Error err -> redirect [ Message.set ~error:[ err ] ]
  in
  Response.handle ~src req result
;;

let validate_tenant = validate `Tenant

module Access : sig
  include module type of Helpers.Access

  val validate : Rock.Middleware.t
end = struct
  include Helpers.Access
  module Guardian = Middleware.Guardian

  let smtp_effects = Guardian.id_effects Email.SmtpAuth.Id.validate Field.Smtp
  let index = Email.Guard.Access.Smtp.index |> Guardian.validate_admin_entity
  let create = Guardian.validate_admin_entity Command.Create.effects
  let read = smtp_effects Email.Guard.Access.Smtp.read
  let update = smtp_effects Command.Update.effects
  let delete = smtp_effects Email.Guard.Access.Smtp.delete
  let validate = create
end
