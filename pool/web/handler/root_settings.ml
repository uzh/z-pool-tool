open Utils.Lwt_result.Infix
module Command = Cqrs_command.Smtp_command
module Field = Pool_common.Message.Field
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module SmtpAuth = Pool_tenant.SmtpAuth

let root_path = "/root"
let settings_path = "/root/settings"
let active_navigation = "/root/settings/smtp"

let show_smtp req =
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, root_path)
    @@
    let flash_fetcher = CCFun.flip Sihl.Web.Flash.find req in
    SmtpAuth.find_by_label database_label
    >|+ Page.Admin.Settings.Smtp.show ~settings_path context flash_fetcher
    >|+ General.create_root_layout ~active_navigation context
    >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let update command success_message req =
  let tags = Logger.req req in
  let result _ =
    let id =
      HttpUtils.get_field_router_param req Field.Smtp |> SmtpAuth.Id.of_string
    in
    let validate = SmtpAuth.find Pool_database.root in
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
      Lwt_list.iter_s (Pool_event.handle_event ~tags Pool_database.root)
    in
    let return_to_overview () =
      HttpUtils.redirect_to_with_actions
        active_navigation
        [ Message.set ~success:[ success_message ] ]
    in
    validate id
    >== events
    >|- (fun err -> err, active_navigation)
    |>> handle
    |>> return_to_overview
  in
  result |> HttpUtils.extract_happy_path req
;;

let update_smtp = update `UpdateDetails Pool_common.Message.SmtpDetailsUpdated

let update_smtp_password =
  update `UpdatePassword Pool_common.Message.SmtpPasswordUpdated
;;

module Access : Helpers.AccessSig = struct
  module Guardian = Middleware.Guardian

  let smtp_effects = Guardian.id_effects SmtpAuth.Id.of_string Field.Smtp
  let read_effects = [ `Read, `TargetEntity `Smtp ]
  let index = Guardian.validate_admin_entity read_effects
  let create = Guardian.validate_admin_entity Command.Create.effects
  let read = Guardian.validate_admin_entity read_effects

  let update =
    [ Command.Update.effects ] |> smtp_effects |> Guardian.validate_generic
  ;;

  let delete = Guardian.denied
end
