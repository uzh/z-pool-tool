open Utils.Lwt_result.Infix
open Http_utils
module Response = Http_response

let src = Logs.Src.create "handler.admin.users"
let id req = get_field_router_param req Pool_message.Field.User

let redirect req =
  let result { Pool_context.database_label; _ } =
    let user_id = id req in
    let open Pool_user in
    let* user =
      user_id
      |> Id.validate
      |> Lwt_result.lift
      >>= find database_label
      >|- Response.not_found
    in
    let redirect =
      match is_admin user with
      | false ->
        let id = Contact.Id.of_string user_id in
        Url.Admin.contact_path ~id ()
      | true ->
        let id = Admin.Id.of_string user_id in
        Url.Admin.admin_path ~id ()
    in
    redirect |> Sihl.Web.externalize_path |> redirect_to |> Lwt_result.ok
  in
  Response.handle ~src req result
;;
