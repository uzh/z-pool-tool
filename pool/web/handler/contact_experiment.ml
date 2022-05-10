module WaitingList = Contact_experiment_waiting_list
module HttpUtils = Http_utils

let create_layout = Contact_general.create_layout

let get_current_contact tenant_db req =
  let open Utils.Lwt_result.Infix in
  Service.User.Web.user_from_session ~ctx:(Pool_tenant.to_ctx tenant_db) req
  ||> CCOption.to_result Pool_common.Message.(NotFound Field.User)
  >>= Contact.find_by_user tenant_db
;;

let index req =
  let open Utils.Lwt_result.Infix in
  let open Lwt_result.Syntax in
  let error_path = "/dashboard" in
  let result context =
    Lwt_result.map_err (fun err -> err, error_path)
    @@
    let tenant_db = context.Pool_context.tenant_db in
    let* contact = get_current_contact tenant_db req in
    let%lwt expermient_list =
      Experiment_type.find_all_public tenant_db contact
    in
    Page.Contact.Experiment.index expermient_list context
    |> create_layout ~active_navigation:"/experiments" req context
    >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let show req =
  let open Utils.Lwt_result.Infix in
  let error_path = "/experiments" in
  let result context =
    Lwt_result.map_err (fun err -> err, error_path)
    @@
    let open Lwt_result.Syntax in
    let tenant_db = context.Pool_context.tenant_db in
    let id =
      Sihl.Web.Router.param req Pool_common.Message.Field.(Id |> show)
      |> Pool_common.Id.of_string
    in
    let* contact = get_current_contact tenant_db req in
    let* Experiment_type.{ experiment; sessions } =
      Experiment_type.find_public_sessions tenant_db id
    in
    let%lwt user_is_enlisted =
      Waiting_list.user_is_enlisted tenant_db contact experiment
    in
    Page.Contact.Experiment.show experiment sessions user_is_enlisted context
    |> Lwt.return_ok
    >>= create_layout req context
    >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;
