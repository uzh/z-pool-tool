open Utils.Lwt_result.Infix
open Http_utils

let src = Logs.Src.create "handler.admin.users"
let extract_happy_path = extract_happy_path ~src
let id req = get_field_router_param req Pool_message.Field.User

let redirect req =
  let result { Pool_context.database_label; _ } =
    Utils.Lwt_result.map_error (fun err -> err, "/error")
    @@
    let user_id = id req in
    let open Pool_user in
    let* redirect =
      user_id
      |> Id.validate
      |> Lwt_result.lift
      >>= find database_label
      >|+ is_admin
      >|+ function
      | false ->
        let id = Contact.Id.of_string user_id in
        Url.Admin.contact_path ~id ()
      | true ->
        let id = Admin.Id.of_string user_id in
        Url.Admin.admin_path ~id ()
    in
    redirect |> Sihl.Web.externalize_path |> redirect_to |> Lwt_result.ok
  in
  result |> extract_happy_path req
;;
