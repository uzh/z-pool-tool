module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Field = Pool_common.Message.Field

let create_layout req = General.create_tenant_layout req

let index req =
  let open Utils.Lwt_result.Infix in
  let id =
    let open Pool_common.Message.Field in
    HttpUtils.find_id Experiment.Id.of_string Experiment req
  in
  let error_path =
    Format.asprintf "/admin/experiments/%s" (Experiment.Id.value id)
  in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, error_path)
    @@ let* experiment = Experiment.find database_label id in
       let* sessions =
         Session.find_all_for_experiment database_label experiment.Experiment.id
         >|+ Session.group_and_sort
         >|+ CCList.flat_map (fun (session, follow_ups) ->
               session :: follow_ups)
       in
       let* assignments =
         Lwt_list.map_s
           (fun session ->
             let* assignments =
               Assignment.find_by_session database_label session.Session.id
             in
             Lwt_result.return (session, assignments))
           sessions
         ||> CCList.all_ok
       in
       Page.Admin.Assignment.list assignments experiment context
       |> create_layout req context
       >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let cancel req =
  let open Utils.Lwt_result.Infix in
  let experiment_id, id =
    let open Pool_common.Message.Field in
    ( HttpUtils.find_id Experiment.Id.of_string Experiment req
    , HttpUtils.find_id Assignment.Id.of_string Assignment req )
  in
  let redirect_path =
    Format.asprintf
      "/admin/experiments/%s/assignments"
      (Experiment.Id.value experiment_id)
  in
  let result { Pool_context.database_label; _ } =
    Utils.Lwt_result.map_error (fun err -> err, redirect_path)
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let* assignment = Assignment.find database_label id in
    let* session =
      Session.find_by_assignment database_label assignment.Assignment.id
    in
    let events =
      let open Cqrs_command.Assignment_command.Cancel in
      handle ~tags (assignment, session) |> Lwt.return
    in
    let handle events =
      let%lwt () =
        Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
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

let mark_as_deleted req =
  let open Utils.Lwt_result.Infix in
  let experiment_id, id =
    let open Pool_common.Message.Field in
    ( HttpUtils.find_id Experiment.Id.of_string Experiment req
    , HttpUtils.find_id Assignment.Id.of_string Assignment req )
  in
  let redirect_path =
    Format.asprintf
      "/admin/experiments/%s/assignments"
      (Experiment.Id.value experiment_id)
  in
  let result { Pool_context.database_label; _ } =
    Utils.Lwt_result.map_error (fun err -> err, redirect_path)
    @@ let* assignment = Assignment.find database_label id in
       let tags = Logger.req req in
       let events =
         Cqrs_command.Assignment_command.MarkAsDeleted.handle ~tags assignment
         |> Lwt.return
       in
       let handle events =
         let%lwt () =
           Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
         in
         Http_utils.redirect_to_with_actions
           redirect_path
           [ Message.set
               ~success:
                 [ Pool_common.Message.(MarkedAsDeleted Field.Assignment) ]
           ]
       in
       events |>> handle
  in
  result |> HttpUtils.extract_happy_path req
;;

module Access : sig
  val index : Rock.Middleware.t
  val cancel : Rock.Middleware.t
  val mark_as_deleted : Rock.Middleware.t
end = struct
  module AssignmentCommand = Cqrs_command.Assignment_command

  let assignment_effects =
    Middleware.Guardian.id_effects Assignment.Id.of_string Field.Assignment
  ;;

  let index =
    Middleware.Guardian.validate_admin_entity
      [ `Read, `TargetEntity `Assignment ]
  ;;

  let cancel =
    [ AssignmentCommand.Cancel.effects ]
    |> assignment_effects
    |> Middleware.Guardian.validate_generic
  ;;

  let mark_as_deleted =
    [ AssignmentCommand.MarkAsDeleted.effects ]
    |> assignment_effects
    |> Middleware.Guardian.validate_generic
  ;;
end
