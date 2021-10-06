module HttpUtils = Http_utils
module Message = HttpUtils.Message

let user_error_to_string = function
  | `Incorrect_password | `Does_not_exist -> "Invalid password provided"
;;

let dashboard_path user =
  let open Lwt_result.Syntax in
  let* is_admin = Admin.user_is_admin user in
  Lwt_result.ok
  @@ ((match is_admin with
      | true -> "/root/tenants"
      | false -> "/")
     |> Lwt.return)
;;

let login_get req =
  let open Lwt_result.Infix in
  let%lwt user = Service.User.Web.user_from_session req in
  let%lwt result =
    match user with
    | Some user ->
      user
      |> dashboard_path
      >|= Sihl.Web.externalize_path
      >|= Sihl.Web.Response.redirect_to
    | None ->
      let csrf = Sihl.Web.Csrf.find req |> Option.get in
      let message =
        Sihl.Web.Flash.find_alert req
        |> CCFun.flip Option.bind Message.of_string
      in
      Page.Public.login csrf message ()
      |> Sihl.Web.Response.of_html
      |> Lwt.return
      |> Lwt_result.ok
  in
  result
  |> CCResult.map_err (fun err -> err, "/")
  |> HttpUtils.extract_happy_path
;;

let login_post req =
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let%lwt result =
    let open Lwt_result.Syntax in
    let login_path = "/login" in
    let* params =
      HttpUtils.urlencoded_to_params urlencoded [ "email"; "password" ]
      |> CCOpt.to_result ("Please provide email and password", login_path)
      |> Lwt_result.lift
    in
    let email = List.assoc "email" params in
    let password = List.assoc "password" params in
    let* user =
      Service.User.login email ~password
      |> Lwt_result.map_err (fun user_result ->
             user_error_to_string user_result, login_path)
    in
    let* success_path =
      dashboard_path user |> Lwt_result.map_err (fun err -> err, login_path)
    in
    Lwt_result.ok
    @@ HttpUtils.redirect_to_with_actions
         success_path
         [ Sihl.Web.Session.set [ "user_id", user.Sihl_user.id ] ]
  in
  result |> HttpUtils.extract_happy_path
;;

(* TODO [timhub]: create middleware or combine get functions *)
let request_reset_password_get req =
  let open Lwt_result.Infix in
  let%lwt user = Service.User.Web.user_from_session req in
  let%lwt result =
    match user with
    | Some user ->
      user
      |> dashboard_path
      >|= Sihl.Web.externalize_path
      >|= Sihl.Web.Response.redirect_to
    | None ->
      let csrf = Sihl.Web.Csrf.find req |> Option.get in
      let message =
        Sihl.Web.Flash.find_alert req
        |> CCFun.flip Option.bind Message.of_string
      in
      Page.Public.request_reset_password csrf message ()
      |> Sihl.Web.Response.of_html
      |> Lwt.return
      |> Lwt_result.ok
  in
  result
  |> CCResult.map_err (fun err -> err, "/")
  |> HttpUtils.extract_happy_path
;;

let logout _ =
  HttpUtils.redirect_to_with_actions
    "/login"
    [ Sihl.Web.Session.set [ "user_id", "" ] ]
;;
