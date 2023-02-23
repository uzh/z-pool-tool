open Utils.Lwt_result.Infix
module Command = Cqrs_command.Smtp_command
module Field = Pool_common.Message.Field
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module SmtpAuth = Pool_tenant.SmtpAuth

let active_navigation = "/admin/settings/schedules"

let show req =
  let result context =
    Schedule.find_all ()
    ||> Page.Admin.Settings.Schedule.index context
    >|> General.create_tenant_layout req ~active_navigation context
    >|+ Sihl.Web.Response.of_html
    >|- fun err -> err, "/"
  in
  result |> HttpUtils.extract_happy_path req
;;

module Access : Helpers.AccessSig = struct
  module Guardian = Middleware.Guardian

  let read_effects = [ `Read, `TargetEntity `Schedule ]
  let index = Guardian.validate_admin_entity read_effects
  let create = Guardian.denied
  let read = Guardian.validate_admin_entity read_effects
  let update = Guardian.denied
  let delete = Guardian.denied
end
