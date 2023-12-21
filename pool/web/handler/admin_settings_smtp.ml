open Utils.Lwt_result.Infix
module Command = Cqrs_command.Smtp_command
module Field = Pool_common.Message.Field
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module SmtpAuth = Email.SmtpAuth

let src = Logs.Src.create "handler.admin.settings_schedule"
let active_navigation = Page.Admin.Settings.Smtp.base_path
let boolean_fields = Field.([ DefaultSmtpServer ] |> CCList.map show)

let smtp_auth_id req =
  let open Pool_common.Message.Field in
  HttpUtils.find_id SmtpAuth.Id.of_string Smtp req
;;

let index req =
  let location = `Tenant in
  let active_navigation = active_navigation location in
  HttpUtils.Htmx.handler
    ~active_navigation
    ~error_path:active_navigation
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

let smtp_form location req =
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, "/")
    @@
    let open Page.Admin.Settings.Smtp in
    let flash_fetcher = CCFun.flip Sihl.Web.Flash.find req in
    let active_navigation = active_navigation location in
    let* html =
      match location with
      | `Tenant ->
        req
        |> smtp_auth_id
        |> SmtpAuth.find database_label
        >|+ show context location flash_fetcher
        >>= General.create_tenant_layout req ~active_navigation context
      | `Root ->
        Pool_tenant.Database.root
        |> SmtpAuth.find_default
        ||> CCResult.to_opt
        ||> (function
               | Some auth -> show context location flash_fetcher auth
               | None -> smtp_create_form context location flash_fetcher)
        >|> General.create_root_layout ~active_navigation context
        ||> CCResult.return
    in
    html |> Sihl.Web.Response.of_html |> Lwt_result.return
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let show = smtp_form `Tenant

let create_post location req =
  let open Command.Create in
  let tags = Pool_context.Logger.Tags.req req in
  let redirect_path = active_navigation location in
  let result { Pool_context.database_label; _ } =
    let validate_label ({ Command.label; _ } as m : Command.create) =
      SmtpAuth.find_by_label database_label label
      ||> function
      | Some _ -> Error (Pool_common.Message.Uniqueness Field.SmtpLabel)
      | None -> Ok m
    in
    let%lwt urlencoded =
      Sihl.Web.Request.to_urlencoded req
      ||> HttpUtils.format_request_boolean_values boolean_fields
      ||> HttpUtils.remove_empty_values
    in
    let%lwt default_smtp = SmtpAuth.find_default_opt database_label in
    let events = handle ~tags default_smtp in
    let handle =
      Lwt_list.iter_s (Pool_event.handle_event ~tags database_label)
    in
    let return_to_overview () =
      HttpUtils.redirect_to_with_actions
        redirect_path
        [ Message.set ~success:[ Pool_common.Message.SmtpConfigurationAdded ] ]
    in
    urlencoded
    |> decode
    |> Lwt_result.lift
    >>= validate_label
    >== events
    >|- (fun err -> err, redirect_path)
    |>> handle
    |>> return_to_overview
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let create = create_post `Tenant

let update_base location command success_message req =
  let tags = Pool_context.Logger.Tags.req req in
  let redirect_path =
    active_navigation location
    |> fun base ->
    match location with
    | `Tenant ->
      Format.asprintf "%s/%s" base (req |> smtp_auth_id |> SmtpAuth.Id.value)
    | `Root -> base
  in
  let result { Pool_context.database_label; _ } =
    Lwt_result.map_error (fun err -> err, redirect_path)
    @@
    let%lwt urlencoded =
      Sihl.Web.Request.to_urlencoded req
      ||> HttpUtils.format_request_boolean_values boolean_fields
      ||> HttpUtils.remove_empty_values
    in
    let* smtp_auth = req |> smtp_auth_id |> SmtpAuth.find database_label in
    let events (_ : SmtpAuth.t) =
      let open CCResult.Infix in
      match command with
      | `UpdateDetails ->
        let%lwt default_smtp = SmtpAuth.find_default_opt database_label in
        Command.Update.(
          decode urlencoded >>= handle ~tags default_smtp smtp_auth)
        |> Lwt_result.lift
      | `UpdatePassword ->
        Command.UpdatePassword.(decode urlencoded >>= handle ~tags smtp_auth)
        |> Lwt_result.lift
    in
    let handle =
      Lwt_list.iter_s (Pool_event.handle_event ~tags database_label)
    in
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
  result |> HttpUtils.extract_happy_path ~src req
;;

let update_password =
  update_base `Tenant `UpdatePassword Pool_common.Message.SmtpPasswordUpdated
;;

let update =
  update_base `Tenant `UpdateDetails Pool_common.Message.SmtpDetailsUpdated
;;

let new_form req =
  let location = `Tenant in
  let active_navigation = active_navigation location in
  let result context =
    Utils.Lwt_result.map_error (fun err -> err, active_navigation)
    @@
    let open Page.Admin.Settings.Smtp in
    let flash_fetcher = CCFun.flip Sihl.Web.Flash.find req in
    smtp_create_form context location flash_fetcher
    |> General.create_tenant_layout req ~active_navigation context
    >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let delete_base location req =
  let tags = Pool_context.Logger.Tags.req req in
  let path = active_navigation location in
  HttpUtils.extract_happy_path ~src req
  @@ fun Pool_context.{ database_label; _ } ->
  let smtp_id =
    Sihl.Web.Router.param req (Field.show Field.Smtp) |> SmtpAuth.Id.of_string
  in
  Cqrs_command.Smtp_command.Delete.handle ~tags smtp_id
  |> Lwt_result.lift
  |>> (fun events ->
        let%lwt () = Pool_event.handle_events ~tags database_label events in
        Http_utils.redirect_to_with_actions
          path
          [ Message.set ~success:[ Pool_common.Message.(Deleted Field.Smtp) ] ])
  |> Utils.Lwt_result.map_error (fun err -> err, path)
;;

let delete = delete_base `Tenant

module Access : module type of Helpers.Access = struct
  include Helpers.Access
  module Guardian = Middleware.Guardian

  let smtp_effects = Guardian.id_effects Email.SmtpAuth.Id.of_string Field.Smtp
  let index = Email.Guard.Access.Smtp.index |> Guardian.validate_admin_entity
  let create = Guardian.validate_admin_entity Command.Create.effects

  let read =
    Email.Guard.Access.Smtp.read |> smtp_effects |> Guardian.validate_generic
  ;;

  let update =
    Command.Update.effects |> smtp_effects |> Guardian.validate_generic
  ;;

  let delete =
    Email.Guard.Access.Smtp.delete |> smtp_effects |> Guardian.validate_generic
  ;;
end
