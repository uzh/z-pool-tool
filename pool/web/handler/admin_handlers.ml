module I18n = Admin_i18n
module Message = Http_utils.Message
module Settings = Admin_settings

let dashboard req =
  let result context =
    let message =
      CCOption.bind (Sihl.Web.Flash.find_alert req) Message.of_string
    in
    Page.Admin.dashboard message context
    |> Sihl.Web.Response.of_html
    |> Lwt_result.return
  in
  result |> Http_utils.extract_happy_path req
;;
