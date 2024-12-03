open CCFun
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Queue = Admin_settings_queue
module ActorPermission = Admin_settings_actor_permissions
module RolePermission = Admin_settings_role_permissions
module Schedule = Admin_settings_schedule
module Smtp = Admin_settings_smtp
module Tags = Admin_settings_tags
module TextMessages = Admin_settings_text_messages

let src = Logs.Src.create "handler.admin.settings"
let create_layout req = General.create_tenant_layout req

let show req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, "/")
    @@
    let languages = Pool_context.Tenant.get_tenant_languages_exn req in
    let%lwt email_suffixes = Settings.find_email_suffixes database_label in
    let%lwt contact_email = Settings.find_contact_email database_label in
    let%lwt inactive_user_disable_after =
      Settings.find_inactive_user_disable_after database_label
    in
    let%lwt inactive_user_warning =
      Settings.find_inactive_user_warning database_label
    in
    let%lwt trigger_profile_update_after =
      Settings.find_trigger_profile_update_after database_label
    in
    let%lwt default_reminder_lead_time =
      Settings.find_default_reminder_lead_time database_label
    in
    let%lwt default_text_msg_reminder_lead_time =
      Settings.find_default_text_msg_reminder_lead_time database_label
    in
    let%lwt user_import_first_reminder =
      Settings.find_user_import_first_reminder_after database_label
    in
    let%lwt user_import_second_reminder =
      Settings.find_user_import_second_reminder_after database_label
    in
    let text_messages_enabled = Pool_context.Tenant.text_messages_enabled req in
    let flash_fetcher key = Sihl.Web.Flash.find key req in
    Page.Admin.Settings.show
      languages
      email_suffixes
      contact_email
      inactive_user_disable_after
      inactive_user_warning
      trigger_profile_update_after
      default_reminder_lead_time
      default_text_msg_reminder_lead_time
      user_import_first_reminder
      user_import_second_reminder
      context
      text_messages_enabled
      flash_fetcher
    |> create_layout req ~active_navigation:"/admin/settings" context
    >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let update_settings req =
  let open Utils.Lwt_result.Infix in
  let open Cqrs_command.Settings_command in
  let lift = Lwt_result.lift in
  let tags = Pool_context.Logger.Tags.req req in
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let redirect_path = "/admin/settings" in
  let result { Pool_context.database_label; user; _ } =
    Utils.Lwt_result.map_error (fun err ->
      err, redirect_path, [ HttpUtils.urlencoded_to_flash urlencoded ])
    @@
    let events () =
      let command_handler =
        let open CCResult.Infix in
        function
        | `UpdateLanguages ->
          fun m ->
            CCList.filter_map
              (fun (k, _) ->
                 match CCList.mem k Pool_common.Language.all_codes with
                 | true -> Some (k |> Pool_common.Language.create)
                 | false -> None)
              m
            |> CCResult.flatten_l
            >>= UpdateLanguages.handle ~tags
            |> lift
        | `UpdateEmailSuffixes -> UpdateEmailSuffixes.handle ~tags %> lift
        | `CreateEmailSuffix ->
          fun m ->
            let%lwt suffixes = Settings.find_email_suffixes database_label in
            CreateEmailSuffix.(m |> decode >>= handle ~tags suffixes) |> lift
        | `DeleteEmailSuffix ->
          fun m ->
            let%lwt suffixes = Settings.find_email_suffixes database_label in
            DeleteEmailSuffix.(m |> decode >>= handle ~tags suffixes) |> lift
        | `UpdateDefaultLeadTime ->
          fun m ->
            UpdateDefaultEmailLeadTime.(m |> decode >>= handle ~tags) |> lift
        | `UpdateTextMsgDefaultLeadTime ->
          fun m ->
            UpdateDefaultTextMessageLeadTime.(m |> decode >>= handle ~tags)
            |> lift
        | `UpdateContactEmail ->
          fun m -> UpdateContactEmail.(m |> decode >>= handle ~tags) |> lift
        | `UpdateInactiveUserDisableAfter ->
          fun m ->
            InactiveUser.DisableAfter.(m |> decode >>= handle ~tags) |> lift
        | `UpdateInactiveUserWarning ->
          fun m -> InactiveUser.Warning.(m |> decode >>= handle ~tags) |> lift
        | `UpdateTriggerProfileUpdateAfter ->
          fun m ->
            UpdateTriggerProfileUpdateAfter.(m |> decode >>= handle ~tags)
            |> lift
        | `UserImportFirstReminderAfter ->
          fun m ->
            UserImportReminder.UpdateFirstReminder.(
              m |> decode >>= handle ~tags)
            |> lift
        | `UserImportSecondReminderAfter ->
          fun m ->
            UserImportReminder.UpdateSecondReminder.(
              m |> decode >>= handle ~tags)
            |> lift
      in
      Sihl.Web.Router.param req "action"
      |> Settings.action_of_param
      |> lift
      >>= flip command_handler urlencoded
    in
    let handle = Pool_event.handle_events ~tags database_label user in
    let return_to_settings () =
      Http_utils.redirect_to_with_actions
        redirect_path
        [ Message.set ~success:[ Pool_message.Success.SettingsUpdated ] ]
    in
    () |> events |>> handle |>> return_to_settings
  in
  result |> HttpUtils.extract_happy_path_with_actions ~src req
;;

module Access : module type of Helpers.Access = struct
  include Helpers.Access
  module Command = Cqrs_command.Settings_command
  module Guardian = Middleware.Guardian

  let update =
    let find_effects = function
      | `CreateEmailSuffix -> Command.CreateEmailSuffix.effects
      | `DeleteEmailSuffix -> Command.DeleteEmailSuffix.effects
      | `UpdateDefaultLeadTime -> Command.UpdateDefaultEmailLeadTime.effects
      | `UpdateTextMsgDefaultLeadTime ->
        Command.UpdateDefaultTextMessageLeadTime.effects
      | `UpdateInactiveUserDisableAfter ->
        Command.InactiveUser.DisableAfter.effects
      | `UpdateInactiveUserWarning -> Command.InactiveUser.Warning.effects
      | `UpdateContactEmail -> Command.UpdateContactEmail.effects
      | `UpdateEmailSuffixes -> Command.UpdateEmailSuffixes.effects
      | `UpdateLanguages -> Command.UpdateLanguages.effects
      | `UpdateTriggerProfileUpdateAfter ->
        Command.UpdateTriggerProfileUpdateAfter.effects
      | `UserImportFirstReminderAfter ->
        Command.UserImportReminder.UpdateFirstReminder.effects
      | `UserImportSecondReminderAfter ->
        Command.UserImportReminder.UpdateSecondReminder.effects
    in
    flip Sihl.Web.Router.param "action"
    %> Settings.action_of_param
    %> CCResult.map find_effects
    |> Guardian.validate_generic
  ;;

  let index = Settings.Guard.Access.index |> Guardian.validate_admin_entity
end
