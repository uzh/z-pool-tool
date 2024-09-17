open Utils.Lwt_result.Infix
module HttpUtils = Http_utils

module Config = struct
  let src = Logs.Src.create "handler.root.user_profile"
  let prefix = "/root"

  let create_layout (_ : Rock.Request.t) ?active_navigation context content =
    General.create_root_layout ?active_navigation context content
    |> Lwt_result.ok
  ;;
end

include Admin_user_profile.MakeUserProfile (Config)

let update_password req =
  let open HttpUtils in
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let result { Pool_context.database_label; user; _ } =
    let tags = Pool_context.Logger.Tags.req req in
    Utils.Lwt_result.map_error (fun msg ->
      msg, active_navigation, [ urlencoded_to_flash urlencoded ])
    @@ let* admin = Pool_context.get_admin_user user |> Lwt_result.lift in
       let* events =
         let open CCResult.Infix in
         let open Cqrs_command.User_command.UpdatePassword in
         decode urlencoded
         >>= handle ~tags Admin.(admin |> id |> Id.to_user)
         |> Lwt_result.lift
       in
       let%lwt () = Pool_event.handle_events ~tags database_label user events in
       redirect_to_with_actions
         active_navigation
         [ Message.set ~success:[ Pool_message.Success.PasswordChanged ] ]
       |> Lwt_result.ok
  in
  result |> extract_happy_path_with_actions ~src req
;;
