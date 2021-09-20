module HttpUtils = Http_utils
module Message = HttpUtils.Message

let tenants req =
  let open Utils.Lwt_result.Infix in
  let error_path = "/admin/payouts" in
  let show_payout_list () =
    let message =
      Sihl.Web.Flash.find_alert req |> CCFun.flip Option.bind Message.of_string
    in
    let csrf = Sihl.Web.Csrf.find req |> Option.get in
    Page.Root.Tenant.list csrf ~message ()
    |> Sihl.Web.Response.of_html
    |> Lwt.return_ok
  in
  show_payout_list ()
  |> Lwt_result.map_err (fun err -> err, error_path)
  >|> HttpUtils.extract_happy_path
;;
