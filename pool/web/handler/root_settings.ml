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
    let open Page.Admin.Settings.Smtp in
    let flash_fetcher = CCFun.flip Sihl.Web.Flash.find req in
    SmtpAuth.find_by_label database_label
    ||> (function
          | Ok smtp -> show ~settings_path context flash_fetcher smtp
          | Error _ -> smtp_create_form ~settings_path context flash_fetcher)
    ||> General.create_root_layout ~active_navigation context
    ||> Sihl.Web.Response.of_html
    >|> Lwt_result.return
  in
  result |> HttpUtils.extract_happy_path req
;;

let create_smtp = Admin_settings.Smtp.create ~redirect_path:active_navigation

let update_smtp =
  Admin_settings.Smtp.update_base
    ~redirect_path:active_navigation
    `UpdateDetails
    Pool_common.Message.SmtpDetailsUpdated
;;

let update_smtp_password =
  Admin_settings.Smtp.update_base
    ~redirect_path:active_navigation
    `UpdatePassword
    Pool_common.Message.SmtpPasswordUpdated
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
