module Message = Http_utils.Message

let dashboard req =
  let message = CCOpt.bind (Sihl.Web.Flash.find_alert req) Message.of_string in
  Page.Admin.dashboard message () |> Sihl.Web.Response.of_html |> Lwt.return
;;
