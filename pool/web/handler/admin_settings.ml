module HttpUtils = Http_utils
module Message = HttpUtils.Message

let create_layout req = General.create_tenant_layout req

let show req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.tenant_db; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, "/")
    @@ let* languages =
         Pool_context.Tenant.get_tenant_languages req |> Lwt_result.lift
       in
       let%lwt email_suffixes = Settings.find_email_suffixes tenant_db in
       let%lwt contact_email = Settings.find_contact_email tenant_db in
       let%lwt inactive_user_disable_after =
         Settings.find_inactive_user_disable_after tenant_db
       in
       let%lwt inactive_user_warning =
         Settings.find_inactive_user_warning tenant_db
       in
       let%lwt trigger_profile_update_after =
         Settings.find_trigger_profile_update_after tenant_db
       in
       let%lwt terms_and_conditions =
         Settings.find_terms_and_conditions tenant_db
       in
       let%lwt default_reminder_lead_time =
         Settings.find_default_reminder_lead_time tenant_db
       in
       let flash_fetcher key = Sihl.Web.Flash.find key req in
       Page.Admin.Settings.show
         languages
         email_suffixes
         contact_email
         inactive_user_disable_after
         inactive_user_warning
         trigger_profile_update_after
         terms_and_conditions
         default_reminder_lead_time
         context
         flash_fetcher
       |> create_layout req ~active_navigation:"/admin/settings" context
       >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let update_settings req =
  let open Utils.Lwt_result.Infix in
  let open Cqrs_command.Settings_command in
  let lift = Lwt_result.lift in
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let tags = Logger.req req in
  let redirect_path = "/admin/settings" in
  let result { Pool_context.tenant_db; _ } =
    Utils.Lwt_result.map_error (fun err ->
      err, redirect_path, [ HttpUtils.urlencoded_to_flash urlencoded ])
    @@ let* system_languages =
         Pool_context.Tenant.get_tenant_languages req |> Lwt_result.lift
       in
       let events () =
         let command_handler =
           let open CCResult.Infix in
           function
           | `UpdateTenantLanguages ->
             fun m ->
               let%lwt terms_and_conditions =
                 Settings.find_terms_and_conditions tenant_db
               in
               m
               |> CCList.filter_map (fun (k, _) ->
                    match CCList.mem k Pool_common.Language.all_codes with
                    | true -> Some (k |> Pool_common.Language.create)
                    | false -> None)
               |> CCResult.flatten_l
               >>= UpdateLanguages.handle ~tags terms_and_conditions
               |> lift
           | `UpdateTenantEmailSuffixes ->
             fun m -> m |> UpdateEmailSuffixes.handle ~tags |> lift
           | `CreateTenantEmailSuffix ->
             fun m ->
               let%lwt suffixes = Settings.find_email_suffixes tenant_db in
               CreateEmailSuffix.(m |> decode >>= handle ~tags suffixes) |> lift
           | `DeleteTenantEmailSuffix ->
             fun m ->
               let%lwt suffixes = Settings.find_email_suffixes tenant_db in
               DeleteEmailSuffix.(m |> decode >>= handle ~tags suffixes) |> lift
           | `UpdateDefaultLeadTime ->
             fun m ->
               UpdateDefaultLeadTime.(m |> decode >>= handle ~tags) |> lift
           | `UpdateTenantContactEmail ->
             fun m -> UpdateContactEmail.(m |> decode >>= handle ~tags) |> lift
           | `UpdateInactiveUserDisableAfter ->
             fun m ->
               InactiveUser.DisableAfter.(m |> decode >>= handle ~tags) |> lift
           | `UpdateInactiveUserWarning ->
             fun m ->
               InactiveUser.Warning.(m |> decode >>= handle ~tags) |> lift
           | `UpdateTermsAndConditions ->
             fun m ->
               UpdateTermsAndConditions.(handle ~tags system_languages m)
               |> lift
           | `UpdateTriggerProfileUpdateAfter ->
             fun m ->
               UpdateTriggerProfileUpdateAfter.(m |> decode >>= handle ~tags)
               |> lift
         in
         Sihl.Web.Router.param req "action"
         |> Settings.action_of_param
         |> lift
         >>= CCFun.flip command_handler urlencoded
       in
       let handle = Lwt_list.iter_s (Pool_event.handle_event ~tags tenant_db) in
       let return_to_settings () =
         Http_utils.redirect_to_with_actions
           redirect_path
           [ Message.set ~success:[ Pool_common.Message.SettingsUpdated ] ]
       in
       () |> events |>> handle |>> return_to_settings
  in
  result |> HttpUtils.extract_happy_path_with_actions req
;;
