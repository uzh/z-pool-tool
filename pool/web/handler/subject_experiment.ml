module HttpUtils = Http_utils

let create_layout = Subject_general.create_layout

let index req =
  let open Utils.Lwt_result.Infix in
  let error_path = "/dashboard" in
  let result context =
    Lwt_result.map_err (fun err -> err, error_path)
    @@
    let tenant_db = context.Pool_context.tenant_db in
    let%lwt expermient_list = Experiment_type.find_all_public tenant_db () in
    Page.Subject.Experiment.index expermient_list context
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
    let* experiment = Experiment_type.find_public tenant_db id in
    Page.Subject.Experiment.show experiment context
    |> Lwt.return_ok
    >>= create_layout req context
    >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;
