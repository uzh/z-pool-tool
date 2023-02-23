let active_navigation = "/root/settings/smtp"

let show_smtp req =
  let open Utils.Lwt_result.Infix in
  let root_path = "/root" in
  let settings_path = "/root/settings" in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, root_path)
    @@
    let open Page.Admin.Settings.Smtp in
    let flash_fetcher = CCFun.flip Sihl.Web.Flash.find req in
    Pool_tenant.SmtpAuth.find_by_label database_label
    ||> (function
          | Ok smtp -> show ~settings_path context flash_fetcher smtp
          | Error _ -> smtp_create_form ~settings_path context flash_fetcher)
    ||> General.create_root_layout ~active_navigation context
    ||> Sihl.Web.Response.of_html
    >|> Lwt_result.return
  in
  result |> Http_utils.extract_happy_path req
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

module Access = Admin_settings.Smtp.Access
