module HttpUtils = Http_utils
module Message = HttpUtils.Message

let create_layout req = General.create_tenant_layout `Admin req

let index req =
  let open Utils.Lwt_result.Infix in
  let error_path = "/admin/dashboard" in
  let result context =
    Lwt_result.map_err (fun err -> err, error_path)
    @@
    let message =
      CCOption.bind (Sihl.Web.Flash.find_alert req) Message.of_string
    in
    let tenant_db = context.Pool_context.tenant_db in
    let%lwt expermient_list = Experiment.find_all tenant_db () in
    Page.Admin.Experiments.index expermient_list context
    |> create_layout req context message
    >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let new_form req =
  let open Utils.Lwt_result.Infix in
  let error_path = "/admin/experiments" in
  let result context =
    Lwt_result.map_err (fun err -> err, error_path)
    @@
    let message =
      CCOption.bind (Sihl.Web.Flash.find_alert req) Message.of_string
    in
    let csrf = HttpUtils.find_csrf req in
    Page.Admin.Experiments.new_form csrf context
    |> create_layout req context message
    >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let create req =
  let open Utils.Lwt_result.Infix in
  let result context =
    Lwt_result.map_err (fun err -> err, "/admin/")
    @@
    let tenant_db = context.Pool_context.tenant_db in
    let events =
      let open CCResult.Infix in
      let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
      urlencoded
      |> Cqrs_command.Experiment_command.Create.decode
      >>= Cqrs_command.Experiment_command.Create.handle
      |> Lwt_result.lift
    in
    let handle events =
      let%lwt (_ : unit list) =
        Lwt_list.map_s (Pool_event.handle_event tenant_db) events
      in
      Http_utils.redirect_to_with_actions
        "/admin/experiments"
        [ Message.set
            ~success:[ Pool_common.Message.(Created Field.Experiment) ]
        ]
    in
    events |>> handle
  in
  result |> HttpUtils.extract_happy_path req
;;
