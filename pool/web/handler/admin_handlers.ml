module Message = Http_utils.Message
module Settings = Admin_settings
module Users = Admin_users

let dashboard req =
  let message =
    CCOption.bind (Sihl.Web.Flash.find_alert req) Message.of_string
  in
  Page.Admin.dashboard message () |> Sihl.Web.Response.of_html |> Lwt.return
;;
