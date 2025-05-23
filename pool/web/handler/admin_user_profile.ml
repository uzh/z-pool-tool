open Utils.Lwt_result.Infix
module Command = Cqrs_command.Admin_command
module HttpUtils = Http_utils
module Field = Pool_message.Field
module Response = Http_response

module Config = struct
  let src = Logs.Src.create "handler.admin.user_profile"
  let prefix = "/admin"
  let create_layout = General.create_tenant_layout
end

module MakeUserProfile (Config : module type of Config) = struct
  include Config

  let active_navigation = Format.asprintf "%s/user/login-information" prefix

  let show req =
    let result ({ Pool_context.database_label; language; user; _ } as context) =
      let* admin =
        Pool_context.get_admin_user user
        |> Lwt_result.lift
        >|- CCFun.const Response.access_denied
      in
      Response.bad_request_render_error context
      @@
      let%lwt password_policy =
        I18n.find_by_key database_label I18n.Key.PasswordPolicyText language
      in
      Page.Admin.login_information ~action_prefix:prefix admin context password_policy
      |> create_layout ~active_navigation req context
      >|+ Sihl.Web.Response.of_html
    in
    Response.handle ~src req result
  ;;

  let update_password req =
    let open HttpUtils in
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    let result { Pool_context.database_label; language; user; _ } =
      let tags = Pool_context.Logger.Tags.req req in
      Response.bad_request_on_error show
      @@ let* admin = Pool_context.get_admin_user user |> Lwt_result.lift in
         let tenant = Pool_context.Tenant.get_tenant_exn req in
         let%lwt notification =
           Message_template.PasswordChange.create language tenant (Admin.user admin)
         in
         let* events =
           let open CCResult.Infix in
           let open Cqrs_command.User_command.UpdatePassword in
           decode urlencoded
           >>= handle ~tags ~notification Admin.(admin |> id |> Id.to_user)
           |> Lwt_result.lift
         in
         let%lwt () = Pool_event.handle_events ~tags database_label user events in
         redirect_to_with_actions
           active_navigation
           [ Message.set ~success:[ Pool_message.Success.PasswordChanged ] ]
         |> Lwt_result.ok
    in
    Response.handle ~src req result
  ;;

  let update_name req =
    let open HttpUtils in
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    let result { Pool_context.database_label; user; _ } =
      let tags = Pool_context.Logger.Tags.req req in
      Response.bad_request_on_error show
      @@ let* admin = Pool_context.get_admin_user user |> Lwt_result.lift in
         let* events =
           let open CCResult.Infix in
           Command.Update.(decode urlencoded >>= handle ~tags admin) |> Lwt_result.lift
         in
         let%lwt () = Pool_event.handle_events ~tags database_label user events in
         redirect_to_with_actions
           active_navigation
           [ Message.set ~success:[ Pool_message.Success.Updated Field.Name ] ]
         |> Lwt_result.ok
    in
    Response.handle ~src req result
  ;;
end

include MakeUserProfile (Config)
