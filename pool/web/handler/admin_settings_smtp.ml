open Utils.Lwt_result.Infix
module Command = Cqrs_command.Smtp_command
module Field = Pool_common.Message.Field
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module SmtpAuth = Pool_tenant.SmtpAuth

let active_navigation = "/admin/settings/smtp"

let show req =
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, "/")
    @@
    let open Page.Admin.Settings.Smtp in
    let flash_fetcher = CCFun.flip Sihl.Web.Flash.find req in
    SmtpAuth.find_by_label database_label
    ||> (function
          | Ok smtp -> show context flash_fetcher smtp
          | Error _ -> smtp_create_form context flash_fetcher)
    >|> General.create_tenant_layout req ~active_navigation context
    >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let create ?(redirect_path = active_navigation) req =
  let tags = Pool_context.Logger.Tags.req req in
  let result { Pool_context.database_label; _ } =
    let validate () =
      SmtpAuth.find_by_label database_label
      ||> function
      | Ok _ -> Error (Pool_common.Message.Invalid Field.Smtp)
      | Error _ -> Ok ()
    in
    let%lwt urlencoded =
      Sihl.Web.Request.to_urlencoded req
      ||> HttpUtils.remove_empty_values
      ||> CCList.cons
            ("smtp_label", [ Pool_database.Label.value database_label ])
    in
    let events () =
      let open CCResult.Infix in
      Command.Create.(decode urlencoded >>= handle ~tags)
    in
    let handle =
      Lwt_list.iter_s (Pool_event.handle_event ~tags database_label)
    in
    let return_to_overview () =
      HttpUtils.redirect_to_with_actions
        redirect_path
        [ Message.set ~success:[ Pool_common.Message.SmtpConfigurationAdded ] ]
    in
    validate ()
    >== events
    >|- (fun err -> err, redirect_path)
    |>> handle
    |>> return_to_overview
  in
  result |> HttpUtils.extract_happy_path req
;;

let update_base ?(redirect_path = active_navigation) command success_message req
  =
  let tags = Pool_context.Logger.Tags.req req in
  let result { Pool_context.database_label; _ } =
    let id =
      HttpUtils.get_field_router_param req Field.Smtp |> SmtpAuth.Id.of_string
    in
    let validate = SmtpAuth.find database_label in
    let%lwt urlencoded =
      Sihl.Web.Request.to_urlencoded req
      ||> HttpUtils.remove_empty_values
      ||> CCList.cons ("id", [ SmtpAuth.Id.value id ])
    in
    let events (_ : SmtpAuth.t) =
      let open CCResult.Infix in
      match command with
      | `UpdateDetails -> Command.Update.(decode urlencoded >>= handle ~tags)
      | `UpdatePassword ->
        Command.UpdatePassword.(decode urlencoded >>= handle ~tags)
    in
    let handle =
      Lwt_list.iter_s (Pool_event.handle_event ~tags database_label)
    in
    let return_to_overview () =
      HttpUtils.redirect_to_with_actions
        redirect_path
        [ Message.set ~success:[ success_message ] ]
    in
    validate id
    >== events
    >|- (fun err -> err, redirect_path)
    |>> handle
    |>> return_to_overview
  in
  result |> HttpUtils.extract_happy_path req
;;

let update_password =
  update_base `UpdatePassword Pool_common.Message.SmtpPasswordUpdated
;;

let update = update_base `UpdateDetails Pool_common.Message.SmtpDetailsUpdated

module Access : Helpers.AccessSig = struct
  module Guardian = Middleware.Guardian

  let smtp_effects =
    Guardian.id_effects Pool_tenant.SmtpAuth.Id.of_string Field.Smtp
  ;;

  let read_effects = [ `Read, `TargetEntity `Smtp ]
  let index = Guardian.validate_admin_entity read_effects
  let create = Guardian.validate_admin_entity Command.Create.effects
  let read = Guardian.validate_admin_entity read_effects

  let update =
    [ Command.Update.effects ] |> smtp_effects |> Guardian.validate_generic
  ;;

  let delete = Guardian.denied
end
