module Message = Http_utils.Message

let index req =
  let open Utils.Lwt_result.Infix in
  let error_path = "/" in
  let show () =
    let message =
      Sihl.Web.Flash.find_alert req |> CCFun.flip Option.bind Message.of_string
    in
    Page.Public.index ~message () |> Sihl.Web.Response.of_html |> Lwt.return_ok
  in
  show ()
  |> Lwt_result.map_err (fun err -> err, error_path)
  >|> Http_utils.extract_happy_path
;;
