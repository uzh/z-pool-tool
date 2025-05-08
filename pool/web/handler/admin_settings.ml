open CCFun
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Queue = Admin_settings_queue
module ActorPermission = Admin_settings_actor_permissions
module Response = Http_response
module RolePermission = Admin_settings_role_permissions
module Schedule = Admin_settings_schedule
module Smtp = Admin_settings_smtp
module Tags = Admin_settings_tags
module TextMessages = Admin_settings_text_messages

let src = Logs.Src.create "handler.admin.settings"
let create_layout req = General.create_tenant_layout req

let settings_page ?open_tab req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.database_label; _ } as context) =
    Response.bad_request_render_error context
    @@
    let open_tab =
      let open CCOption in
      open_tab
      <+> (Sihl.Web.Request.query "action" req
           >>= CCFun.(Settings.action_of_param %> of_result))
    in
    let languages = Pool_context.Tenant.get_tenant_languages_exn req in
    let%lwt email_suffixes = Settings.find_email_suffixes database_label in
    let%lwt contact_email = Settings.find_contact_email database_label in
    let%lwt inactive_user_disable_after =
      Settings.find_inactive_user_disable_after database_label
    in
    let%lwt inactive_user_warning = Settings.find_inactive_user_warning database_label in
    let%lwt inactive_user_service_disabled =
      Settings.find_inactive_user_service_disabled database_label
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
    let%lwt page_scripts = Settings.PageScript.find database_label in
    let%lwt text_messages_enabled = Pool_context.Tenant.text_messages_enabled req in
    Page.Admin.Settings.show
      ?open_tab
      languages
      email_suffixes
      contact_email
      inactive_user_disable_after
      inactive_user_warning
      inactive_user_service_disabled
      trigger_profile_update_after
      default_reminder_lead_time
      default_text_msg_reminder_lead_time
      user_import_first_reminder
      user_import_second_reminder
      page_scripts
      context
      text_messages_enabled
    |> create_layout req ~active_navigation:"/admin/settings" context
    >|+ Sihl.Web.Response.of_html
  in
  Response.handle ~src req result
;;

let show req = settings_page req

let update_settings req =
  let open Utils.Lwt_result.Infix in
  let open Cqrs_command.Settings_command in
  let lift = Lwt_result.lift in
  let tags = Pool_context.Logger.Tags.req req in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
  in
  let result { Pool_context.database_label; user; _ } =
    let* action =
      Sihl.Web.Router.param req "action"
      |> Settings.action_of_param
      |> Lwt_result.lift
      |> Response.bad_request_on_error ~urlencoded show
    in
    Response.bad_request_on_error ~urlencoded (settings_page ~open_tab:action)
    @@
    let redirect_path = HttpUtils.Url.Admin.settings_path_with_action_param action in
    let events () =
      let command_handler urlencoded =
        let open CCResult.Infix in
        function
        | `UpdateLanguages ->
          CCList.filter_map
            (fun (k, _) ->
               match CCList.mem k Pool_common.Language.all_codes with
               | true -> Some (k |> Pool_common.Language.create)
               | false -> None)
            urlencoded
          |> CCResult.flatten_l
          >>= UpdateLanguages.handle ~tags
          |> lift
        | `UpdateEmailSuffixes -> UpdateEmailSuffixes.handle ~tags urlencoded |> lift
        | `CreateEmailSuffix ->
          let%lwt suffixes = Settings.find_email_suffixes database_label in
          CreateEmailSuffix.(urlencoded |> decode >>= handle ~tags suffixes) |> lift
        | `UpdateDefaultLeadTime ->
          UpdateDefaultEmailLeadTime.(urlencoded |> decode >>= handle ~tags) |> lift
        | `UpdateTextMsgDefaultLeadTime ->
          UpdateDefaultTextMessageLeadTime.(urlencoded |> decode >>= handle ~tags) |> lift
        | `UpdateContactEmail ->
          UpdateContactEmail.(urlencoded |> decode >>= handle ~tags) |> lift
        | `UpdateInactiveUserDisableAfter ->
          InactiveUser.DisableAfter.(urlencoded |> decode >>= handle ~tags) |> lift
        | `UpdateInactiveUserWarning ->
          let open Pool_message in
          let urlencoded_list field = Sihl.Web.Request.urlencoded_list field req in
          let%lwt values = urlencoded_list Field.(show InactiveUserWarning) in
          let%lwt units = urlencoded_list Field.(show (TimeUnitOf InactiveUserWarning)) in
          InactiveUser.Warning.(handle ~tags ~values ~units ()) |> lift
        | `UpdateUnactiveUserServiceDisabled ->
          InactiveUser.DisableService.(urlencoded |> decode >>= handle ~tags) |> lift
        | `UpdateTriggerProfileUpdateAfter ->
          UpdateTriggerProfileUpdateAfter.(urlencoded |> decode >>= handle ~tags) |> lift
        | `UserImportFirstReminderAfter ->
          UserImportReminder.UpdateFirstReminder.(urlencoded |> decode >>= handle ~tags)
          |> lift
        | `UserImportSecondReminderAfter ->
          UserImportReminder.UpdateSecondReminder.(urlencoded |> decode >>= handle ~tags)
          |> lift
        | `UpdateHeadScripts ->
          let location = Settings.PageScript.Head in
          UpdatePageScript.(urlencoded |> decode location >>= handle ~tags location)
          |> lift
        | `UpdateBodyScripts ->
          let location = Settings.PageScript.Body in
          UpdatePageScript.(urlencoded |> decode location >>= handle ~tags location)
          |> lift
      in
      command_handler urlencoded action
    in
    let handle = Pool_event.handle_events ~tags database_label user in
    let return_to_settings () =
      Http_utils.redirect_to_with_actions
        redirect_path
        [ Message.set ~success:[ Pool_message.Success.SettingsUpdated ] ]
    in
    () |> events |>> handle |>> return_to_settings
  in
  Response.handle ~src req result
;;

let inactive_user_warning_subform req =
  let result { Pool_context.language; _ } =
    Page.Admin.Settings.Partials.inactive_user_warning_input language None
    |> HttpUtils.Htmx.html_to_plain_text_response ~status:200
    |> Lwt_result.return
  in
  Response.Htmx.handle ~src req result
;;

let email_suffix_subform req =
  let result { Pool_context.language; _ } =
    Page.Admin.Settings.Partials.email_suffix_input language
    |> HttpUtils.Htmx.html_to_plain_text_response ~status:200
    |> Lwt_result.return
  in
  Response.Htmx.handle ~src req result
;;

let changelog req =
  let result { Pool_context.database_label; _ } =
    let key =
      Http_utils.get_field_router_param req Pool_message.Field.Key |> Settings.Key.read
    in
    let url = Http_utils.Url.Admin.system_settings_changelog_path key in
    let%lwt id = Settings.id_by_key database_label key in
    Lwt_result.ok @@ Helpers.Changelog.htmx_handler ~url id req
  in
  Response.Htmx.handle ~error_as_notification:true req result
;;

let open_changelog_modal req =
  let result ({ Pool_context.database_label; _ } as context) =
    let key =
      Http_utils.get_field_router_param req Pool_message.Field.Key |> Settings.Key.read
    in
    let%lwt id = Settings.id_by_key database_label key in
    let%lwt changelogs =
      let open Changelog in
      let query = Query.from_request ~default:default_query req in
      all_by_entity ~query database_label id
    in
    Page.Admin.Settings.settings_changelog_modal context key changelogs
    |> Response.Htmx.html_to_plain_text_response
    |> Lwt_result.return
  in
  Response.Htmx.handle ~error_as_notification:true req result
;;

module PageScripts = struct
  let location req =
    Http_utils.get_field_router_param req Pool_message.Field.Location
    |> Settings.PageScript.read_location
  ;;

  let changelog req =
    let result { Pool_context.database_label; _ } =
      let location = location req in
      let url = Http_utils.Url.Admin.page_script_changelog_path location in
      let%lwt id = Settings.PageScript.find_id database_label location in
      Lwt_result.ok @@ Helpers.Changelog.htmx_handler ~url id req
    in
    Response.Htmx.handle ~error_as_notification:true req result
  ;;

  let open_changelog_modal req =
    let result ({ Pool_context.database_label; _ } as context) =
      let location = location req in
      let%lwt id = Settings.PageScript.find_id database_label location in
      let%lwt changelogs =
        let open Changelog in
        let query = Query.from_request ~default:default_query req in
        all_by_entity ~query database_label id
      in
      Page.Admin.Settings.page_scripts_changelog_modal context location changelogs
      |> HttpUtils.Htmx.html_to_plain_text_response
      |> Lwt_result.return
    in
    Response.Htmx.handle ~error_as_notification:true req result
  ;;
end

module Access : module type of Helpers.Access = struct
  include Helpers.Access
  module Command = Cqrs_command.Settings_command
  module Guardian = Middleware.Guardian

  let update =
    let find_effects = function
      | `CreateEmailSuffix -> Command.CreateEmailSuffix.effects
      | `UpdateDefaultLeadTime -> Command.UpdateDefaultEmailLeadTime.effects
      | `UpdateTextMsgDefaultLeadTime -> Command.UpdateDefaultTextMessageLeadTime.effects
      | `UpdateInactiveUserDisableAfter -> Command.InactiveUser.DisableAfter.effects
      | `UpdateInactiveUserWarning -> Command.InactiveUser.Warning.effects
      | `UpdateUnactiveUserServiceDisabled -> Command.InactiveUser.DisableService.effects
      | `UpdateContactEmail -> Command.UpdateContactEmail.effects
      | `UpdateEmailSuffixes -> Command.UpdateEmailSuffixes.effects
      | `UpdateLanguages -> Command.UpdateLanguages.effects
      | `UpdateTriggerProfileUpdateAfter ->
        Command.UpdateTriggerProfileUpdateAfter.effects
      | `UserImportFirstReminderAfter ->
        Command.UserImportReminder.UpdateFirstReminder.effects
      | `UserImportSecondReminderAfter ->
        Command.UserImportReminder.UpdateSecondReminder.effects
      | `UpdateHeadScripts | `UpdateBodyScripts -> Command.UpdatePageScript.effects
    in
    flip Sihl.Web.Router.param "action"
    %> Settings.action_of_param
    %> CCResult.map find_effects
    |> Guardian.validate_generic
  ;;

  let index = Settings.Guard.Access.index |> Guardian.validate_admin_entity
end
