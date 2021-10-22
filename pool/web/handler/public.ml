module Message = Http_utils.Message
module Login = Public_login
module Database = Pool_common.Database

let index req =
  let open Utils.Lwt_result.Infix in
  let error_path = "/" in
  let show () =
    let message =
      Sihl.Web.Flash.find_alert req |> CCFun.flip CCOpt.bind Message.of_string
    in
    Page.Public.index message () |> Sihl.Web.Response.of_html |> Lwt.return_ok
  in
  show ()
  |> Lwt_result.map_err (fun err -> err, error_path)
  >|> Http_utils.extract_happy_path
;;

let not_found _ =
  let html = Page.Utils.error_page_not_found () in
  Sihl.Web.Response.of_html html |> Lwt.return
;;

let asset req =
  let open CCResult.Infix in
  let asset_id = Sihl.Web.Router.param req "id" in
  let%lwt file =
    Service.Storage.find
      ~ctx:[ "pool", Database.root |> Pool_common.Database.Label.value ]
      ~id:asset_id
  in
  let mime () = Sihl.Web.Router.param req "filename" |> Http_utils.File.mime in
  let%lwt content = Service.Storage.download_data_base64 file in
  let content = content |> Base64.decode_exn in
  let show mime =
    Sihl.Web.Response.of_plain_text content
    |> Sihl.Web.Response.set_content_type mime
  in
  ()
  |> mime
  >|= show
  |> CCResult.map_err (fun err -> err, "/") (* TODO [timhub]: error handling *)
  |> Http_utils.extract_happy_path
;;
