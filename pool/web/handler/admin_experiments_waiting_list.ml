module HttpUtils = Http_utils
module Message = HttpUtils.Message

let create_layout req = General.create_tenant_layout `Admin req

let index req =
  let open Utils.Lwt_result.Infix in
  let id =
    Pool_common.Message.Field.(Experiment |> show)
    |> Sihl.Web.Router.param req
    |> Pool_common.Id.of_string
  in
  let error_path =
    Format.asprintf "/admin/experiments/%s" (Pool_common.Id.value id)
  in
  let result context =
    let open Lwt_result.Syntax in
    Lwt_result.map_err (fun err -> err, error_path)
    @@
    let tenant_db = context.Pool_context.tenant_db in
    let* waiting_list = Waiting_list.find_by_experiment tenant_db id in
    Page.Admin.Experiments.waiting_list waiting_list context
    |> create_layout req context
    >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let detail req =
  let open Utils.Lwt_result.Infix in
  let id =
    HttpUtils.get_field_router_param req Pool_common.Message.Field.WaitingList
  in
  let experiment_id =
    HttpUtils.get_field_router_param req Pool_common.Message.Field.Experiment
  in
  let error_path =
    Format.asprintf
      "/admin/experiments/%s/waiting-list"
      (Pool_common.Id.value experiment_id)
  in
  let result context =
    let open Lwt_result.Syntax in
    Lwt_result.map_err (fun err -> err, error_path)
    @@
    let tenant_db = context.Pool_context.tenant_db in
    let* waiting_list = Waiting_list.find tenant_db id in
    Page.Admin.WaitingList.detail waiting_list context
    |> create_layout req context
    >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let update req =
  let open Utils.Lwt_result.Infix in
  let experiment_id =
    HttpUtils.get_field_router_param req Pool_common.Message.Field.Experiment
  in
  let waiting_list_id =
    HttpUtils.get_field_router_param req Pool_common.Message.Field.WaitingList
  in
  Logs.info (fun m -> m "%s" Pool_common.Id.(value waiting_list_id));
  let redirect_path =
    let open Pool_common.Id in
    Format.asprintf
      "/admin/experiments/%s/waiting-list/%s"
      (value experiment_id)
      (value waiting_list_id)
  in
  let result context =
    let open Lwt_result.Syntax in
    Lwt_result.map_err (fun err -> err, redirect_path)
    @@
    let tenant_db = context.Pool_context.tenant_db in
    let* waiting_list = Waiting_list.find tenant_db waiting_list_id in
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    let events =
      let open Cqrs_command.Waiting_list_command in
      let open CCResult in
      urlencoded
      |> Update.decode
      >>= Update.handle waiting_list
      |> Lwt_result.lift
    in
    let handle events =
      let%lwt (_ : unit list) =
        Lwt_list.map_s (Pool_event.handle_event tenant_db) events
      in
      Http_utils.redirect_to_with_actions
        redirect_path
        [ Message.set
            ~success:[ Pool_common.Message.(Updated Field.WaitingList) ]
        ]
    in
    events |>> handle
  in
  result |> HttpUtils.extract_happy_path req
;;
