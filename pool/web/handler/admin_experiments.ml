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
    Page.Admin.Experiments.form csrf context
    |> create_layout req context message
    >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let create req =
  let open Utils.Lwt_result.Infix in
  let result context =
    Lwt_result.map_err (fun err -> err, "/admin/experiments/new")
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

let detail edit req =
  let open Utils.Lwt_result.Infix in
  let error_path = "/admin/experiments" in
  let result context =
    Lwt_result.map_err (fun err -> err, error_path)
    @@
    let open Lwt_result.Syntax in
    let message =
      CCOption.bind (Sihl.Web.Flash.find_alert req) Message.of_string
    in
    let tenant_db = context.Pool_context.tenant_db in
    let id =
      Sihl.Web.Router.param req Pool_common.Message.Field.(Id |> show)
      |> Pool_common.Id.of_string
    in
    let* experiment = Experiment.find tenant_db id in
    (match edit with
    | false ->
      let* session_count = Experiment.session_count tenant_db id in
      Page.Admin.Experiments.detail experiment session_count context
      |> Lwt.return_ok
    | true ->
      let csrf = HttpUtils.find_csrf req in
      Page.Admin.Experiments.form ~experiment csrf context |> Lwt.return_ok)
    >>= create_layout req context message
    >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let show = detail false
let edit = detail true

let update req =
  let open Utils.Lwt_result.Infix in
  let result context =
    let id =
      Sihl.Web.Router.param req Pool_common.Message.Field.(Id |> show)
      |> Pool_common.Id.of_string
    in
    let detail_path =
      Format.asprintf "/admin/experiments/%s" (id |> Pool_common.Id.value)
    in
    Lwt_result.map_err (fun err -> err, Format.asprintf "%s/edit" detail_path)
    @@
    let open Lwt_result.Syntax in
    let tenant_db = context.Pool_context.tenant_db in
    let* experiment = Experiment.find tenant_db id in
    let events =
      let open CCResult.Infix in
      let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
      urlencoded
      |> Cqrs_command.Experiment_command.Update.decode
      >>= Cqrs_command.Experiment_command.Update.handle experiment
      |> Lwt_result.lift
    in
    let handle events =
      let%lwt (_ : unit list) =
        Lwt_list.map_s (Pool_event.handle_event tenant_db) events
      in
      Http_utils.redirect_to_with_actions
        detail_path
        [ Message.set
            ~success:[ Pool_common.Message.(Updated Field.Experiment) ]
        ]
    in
    events |>> handle
  in
  result |> HttpUtils.extract_happy_path req
;;

let delete req =
  let open Utils.Lwt_result.Infix in
  let result context =
    let open Lwt_result.Syntax in
    let experiment_id =
      Sihl.Web.Router.param req Pool_common.Message.Field.(Id |> show)
      |> Pool_common.Id.of_string
    in
    let experiments_path = "/admin/experiments" in
    Lwt_result.map_err (fun err ->
        ( err
        , Format.asprintf
            "%s/%s"
            experiments_path
            (Pool_common.Id.value experiment_id) ))
    @@
    let tenant_db = context.Pool_context.tenant_db in
    let* session_count = Experiment.session_count tenant_db experiment_id in
    let events =
      Cqrs_command.Experiment_command.Delete.(
        handle { experiment_id; session_count })
      |> Lwt_result.lift
    in
    let handle events =
      let%lwt (_ : unit list) =
        Lwt_list.map_s (Pool_event.handle_event tenant_db) events
      in
      Http_utils.redirect_to_with_actions
        experiments_path
        [ Message.set
            ~success:[ Pool_common.Message.(Created Field.Experiment) ]
        ]
    in
    events |>> handle
  in
  result |> HttpUtils.extract_happy_path req
;;
