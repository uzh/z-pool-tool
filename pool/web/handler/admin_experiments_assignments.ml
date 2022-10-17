module HttpUtils = Http_utils
module Message = HttpUtils.Message

let create_layout req = General.create_tenant_layout req

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
  let result ({ Pool_context.tenant_db; _ } as context) =
    let open Lwt_result.Syntax in
    Lwt_result.map_error (fun err -> err, error_path)
    @@ let* experiment = Experiment.find tenant_db id in
       let* sessions =
         Session.find_all_for_experiment tenant_db experiment.Experiment.id
       in
       let* assignments =
         Lwt_list.map_s
           (fun session ->
             let* assignments =
               Assignment.find_by_session tenant_db session.Session.id
             in
             Lwt_result.return (session, assignments))
           sessions
         |> Lwt.map CCList.all_ok
       in
       Page.Admin.Assignment.list assignments experiment context
       |> create_layout req context
       >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let cancel req =
  let open Utils.Lwt_result.Infix in
  let experiment_id, id =
    let open Pool_common.Message.Field in
    let open HttpUtils in
    ( get_field_router_param req Experiment |> Pool_common.Id.of_string
    , get_field_router_param req Assignment |> Pool_common.Id.of_string )
  in
  let redirect_path =
    Format.asprintf
      "/admin/experiments/%s/assignments"
      (Pool_common.Id.value experiment_id)
  in
  let result { Pool_context.tenant_db; _ } =
    let open Lwt_result.Syntax in
    Lwt_result.map_error (fun err -> err, redirect_path)
    @@ let* assignment = Assignment.find tenant_db id in
       let events =
         Cqrs_command.Assignment_command.Cancel.handle assignment |> Lwt.return
       in
       let handle events =
         let%lwt (_ : unit list) =
           Lwt_list.map_s (Pool_event.handle_event tenant_db) events
         in
         Http_utils.redirect_to_with_actions
           redirect_path
           [ Message.set
               ~success:[ Pool_common.Message.(Canceled Field.Assignment) ]
           ]
       in
       events |>> handle
  in
  result |> HttpUtils.extract_happy_path req
;;
