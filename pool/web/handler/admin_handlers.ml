module Message = Http_utils.Message

let dashboard req =
  let message =
    Sihl.Web.Flash.find_alert req |> CCFun.flip CCOpt.bind Message.of_string
  in
  Page.Admin.dashboard message () |> Sihl.Web.Response.of_html |> Lwt.return
;;
