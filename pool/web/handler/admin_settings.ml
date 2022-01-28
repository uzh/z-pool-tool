module HttpUtils = Http_utils
module Message = HttpUtils.Message

let show req =
  let%lwt result =
    Lwt_result.map_err (fun err -> err, "/")
    @@
    let open Lwt_result.Syntax in
    let message =
      CCOption.bind (Sihl.Web.Flash.find_alert req) Message.of_string
    in
    let csrf = HttpUtils.find_csrf req in
    let* db = Middleware.Tenant.tenant_db_of_request req in
    let%lwt languages = Settings.find_languages db in
    let%lwt email_suffixes = Settings.find_email_suffixes db in
    let%lwt contact_email = Settings.find_contact_email db in
    let%lwt inactive_user_disable_after =
      Settings.find_inactive_user_disable_after db
    in
    let%lwt inactive_user_warning = Settings.find_inactive_user_warning db in
    let%lwt terms_and_conditions = Settings.find_terms_and_conditions db in
    Page.Admin.Settings.show
      csrf
      languages
      email_suffixes
      contact_email
      inactive_user_disable_after
      inactive_user_warning
      terms_and_conditions
      message
      ()
    |> Sihl.Web.Response.of_html
    |> Lwt.return_ok
  in
  result |> HttpUtils.extract_happy_path
;;

let update_settings req =
  let open Utils.Lwt_result.Infix in
  let open Cqrs_command.Settings_command in
  let lift = Lwt_result.lift in
  let redirect_path = "/admin/settings" in
  let%lwt result =
    Lwt_result.map_err (fun err -> err, redirect_path)
    @@
    let open Lwt_result.Syntax in
    let* tenant_db = Middleware.Tenant.tenant_db_of_request req in
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
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
                   match CCList.mem k (Pool_common.Language.all_codes ()) with
                   | true -> Some (k |> Pool_common.Language.of_string)
                   | false -> None)
            |> CCResult.flatten_l
            >>= UpdateLanguages.handle terms_and_conditions
            |> lift
        | `UpdateTenantEmailSuffixes ->
          fun m -> m |> UpdateEmailSuffixes.handle |> lift
        | `CreateTenantEmailSuffix ->
          fun m ->
            let%lwt suffixes = Settings.find_email_suffixes tenant_db in
            CreateEmailSuffix.(m |> decode >>= handle suffixes) |> lift
        | `DeleteTenantEmailSuffix ->
          fun m ->
            let%lwt suffixes = Settings.find_email_suffixes tenant_db in
            DeleteEmailSuffix.(m |> decode >>= handle suffixes) |> lift
        | `UpdateTenantContactEmail ->
          fun m -> UpdateContactEmail.(m |> decode >>= handle) |> lift
        | `UpdateInactiveUserDisableAfter ->
          fun m -> InactiveUser.DisableAfter.(m |> decode >>= handle) |> lift
        | `UpdateInactiveUserWarning ->
          fun m -> InactiveUser.Warning.(m |> decode >>= handle) |> lift
        | `UpdateTermsAndConditions ->
          fun m ->
            let%lwt languages = Settings.find_languages tenant_db in
            UpdateTermsAndConditions.(handle languages m) |> lift
      in
      Sihl.Web.Router.param req "action"
      |> Settings.action_of_param
      |> lift
      >>= CCFun.flip command_handler urlencoded
    in
    let handle = Lwt_list.iter_s (Pool_event.handle_event tenant_db) in
    let return_to_settings () =
      Http_utils.redirect_to_with_actions
        redirect_path
        [ Message.set ~success:[ Pool_common.Message.SettingsUpdated ] ]
    in
    () |> events |>> handle |>> return_to_settings
  in
  result |> HttpUtils.extract_happy_path
;;
