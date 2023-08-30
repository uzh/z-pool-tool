module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Field = Pool_common.Message.Field

let src = Logs.Src.create "handler.admin.experiments_assignments"
let create_layout req = General.create_tenant_layout req
let experiment_id = HttpUtils.find_id Experiment.Id.of_string Field.Experiment
let session_id = HttpUtils.find_id Session.Id.of_string Field.Session
let assignment_id = HttpUtils.find_id Assignment.Id.of_string Field.Assignment

let list ?(marked_as_deleted = false) req =
  let open Utils.Lwt_result.Infix in
  let id =
    let open Pool_common.Message.Field in
    HttpUtils.find_id Experiment.Id.of_string Experiment req
  in
  let error_path =
    Format.asprintf "/admin/experiments/%s" (Experiment.Id.value id)
  in
  let result ({ Pool_context.database_label; user; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, error_path)
    @@
    let%lwt access_contact_profiles =
      Helpers.Guard.can_access_contact_profile database_label user
    in
    let%lwt view_contact_name =
      Helpers.Guard.can_read_contact_name database_label user
    in
    let%lwt view_contact_info =
      Helpers.Guard.can_read_contact_info database_label user
    in
    let* experiment = Experiment.find database_label id in
    let* sessions =
      Session.find_all_for_experiment database_label experiment.Experiment.id
      >|+ Session.group_and_sort
      >|+ CCList.flat_map (fun (session, follow_ups) -> session :: follow_ups)
    in
    let* html =
      match marked_as_deleted with
      | false ->
        Lwt_list.map_s
          (fun session ->
            let* assignments =
              Assignment.find_by_session database_label session.Session.id
            in
            Lwt_result.return (session, assignments))
          sessions
        ||> CCList.all_ok
        >|+ Page.Admin.Assignment.list
              ~access_contact_profiles
              ~view_contact_name
              ~view_contact_info
              experiment
              context
      | true ->
        Lwt_list.fold_left_s
          (fun res session ->
            res
            |> Lwt_result.lift
            >>= fun sessions ->
            Assignment.find_deleted_by_session database_label session.Session.id
            >|+ function
            | [] -> sessions
            | assignments -> sessions @ [ session, assignments ])
          (Ok [])
          sessions
        >|+ Page.Admin.Assignment.marked_as_deleted
              ~access_contact_profiles
              ~view_contact_name
              ~view_contact_info
              experiment
              context
    in
    html >|> create_layout req context >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let index req = list req
let deleted req = list ~marked_as_deleted:true req

let ids_from_request req =
  let open Pool_common.Message.Field in
  HttpUtils.(
    ( find_id Experiment.Id.of_string Experiment req
    , find_id Session.Id.of_string Session req
    , find_id Assignment.Id.of_string Assignment req ))
;;

let ids_and_redirect_from_req req =
  let open Pool_common in
  let experiment_id, session_id, assignment_id = ids_from_request req in
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let redirect =
    let open Page.Admin.Assignment in
    let open CCOption in
    let experiment_path =
      Format.asprintf
        "/admin/experiments/%s"
        (experiment_id |> Experiment.Id.value)
    in
    let to_path =
      let path = Format.asprintf "%s/%s" experiment_path in
      function
      | Assignments -> path "assignments"
      | DeletedAssignments -> path "assignments/deleted"
      | Session ->
        Format.asprintf "sessions/%s" (session_id |> Session.Id.value) |> path
    in
    CCList.assoc ~eq:CCString.equal Message.Field.(show Redirect) urlencoded
    |> CCList.head_opt
    >>= Page.Admin.Assignment.read_assignment_redirect
    >|= to_path
    |> value ~default:experiment_path
  in
  Lwt.return (experiment_id, session_id, assignment_id, redirect)
;;

let cancel req =
  let open Utils.Lwt_result.Infix in
  let%lwt _, session_id, assignment_id, redirect_path =
    ids_and_redirect_from_req req
  in
  let result { Pool_context.database_label; _ } =
    Utils.Lwt_result.map_error (fun err -> err, redirect_path)
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let* assignments =
      Assignment.find_with_follow_ups database_label assignment_id
    in
    let* session = Session.find database_label session_id in
    let events =
      Cqrs_command.Assignment_command.Cancel.handle ~tags (assignments, session)
      |> Lwt.return
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
  result |> HttpUtils.extract_happy_path ~src req
;;

let mark_as_deleted req =
  let open Utils.Lwt_result.Infix in
  let%lwt experiment_id, _, assignment_id, redirect_path =
    ids_and_redirect_from_req req
  in
  let result { Pool_context.database_label; _ } =
    Utils.Lwt_result.map_error (fun err -> err, redirect_path)
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let* assignments =
      Assignment.find_with_follow_ups database_label assignment_id
    in
    let events =
      match assignments with
      | [] -> Lwt_result.return []
      | hd :: _ as assignments ->
        let* decrement_num_participations =
          Assignment.(
            contact_participation_in_other_assignments
              database_label
              ~exclude_assignments:assignments
              experiment_id
              (Contact.id hd.contact)
            >|+ not
            >|+ IncrementParticipationCount.create)
        in
        Cqrs_command.Assignment_command.MarkAsDeleted.handle
          ~tags
          (hd.Assignment.contact, assignments, decrement_num_participations)
        |> Lwt.return
    in
    let handle events =
      let%lwt () =
        Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
      in
      Http_utils.redirect_to_with_actions
        redirect_path
        [ Message.set
            ~success:[ Pool_common.Message.(MarkedAsDeleted Field.Assignment) ]
        ]
    in
    events |>> handle
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let close_htmx req =
  let tags = Pool_context.Logger.Tags.req req in
  let experiment_id = experiment_id req in
  let session_id = session_id req in
  let assignment_id = assignment_id req in
  let result ({ Pool_context.database_label; user; _ } as context) =
    let open Cqrs_command.Assignment_command in
    let open Utils.Lwt_result.Infix in
    let boolean_fields = Assignment.boolean_fields |> CCList.map Field.show in
    let%lwt urlencoded =
      Sihl.Web.Request.to_urlencoded req
      ||> HttpUtils.format_request_boolean_values boolean_fields
      ||> HttpUtils.remove_empty_values
    in
    let%lwt view_contact_name =
      Helpers.Guard.can_read_contact_name database_label user
    in
    let* experiment = Experiment.find database_label experiment_id in
    let* session = Session.find database_label session_id in
    let* assignment = Assignment.find database_label assignment_id in
    let* updated_assignment, event, no_show, participated, external_data_id =
      let open CCResult.Infix in
      urlencoded
      |> decode_update
      >>= (fun { no_show; participated; external_data_id } ->
            let assignment =
              Assignment.
                { assignment with
                  no_show = Some no_show
                ; participated = Some participated
                ; external_data_id
                }
            in
            let events =
              Assignment.(Updated assignment) |> Pool_event.assignment
            in
            Ok (assignment, events, no_show, participated, external_data_id))
      |> Lwt_result.lift
    in
    let%lwt () = Pool_event.handle_event ~tags database_label event in
    let* counters = Assignment.counters_of_session database_label session_id in
    let updated_fields =
      let open Assignment in
      let fields =
        if assignment.no_show
           |> CCOption.map_or ~default:false NoShow.value
           = NoShow.value no_show
        then []
        else [ Field.NoShow ]
      in
      let fields =
        if assignment.participated
           |> CCOption.map_or ~default:false Participated.value
           = Participated.value participated
        then fields
        else Field.Participated :: fields
      in
      match assignment.external_data_id, external_data_id with
      | None, None -> fields
      | Some id1, Some id2 when ExternalDataId.equal id1 id2 -> fields
      | _ -> Field.ExternalDataId :: fields
    in
    Page.Admin.Session.close_assignment_htmx_row
      ~counters
      ~updated_fields
      context
      experiment
      ~view_contact_name
      session
      updated_assignment
    |> HttpUtils.Htmx.html_to_plain_text_response
    |> Lwt_result.return
  in
  result
  |> HttpUtils.Htmx.handle_error_message ~error_as_notification:true ~src req
;;

let edit req =
  let open Utils.Lwt_result.Infix in
  let experiment_id, session_id, assignment_id = ids_from_request req in
  let redirect_path =
    Page.Admin.Session.session_path experiment_id session_id
  in
  let result ({ Pool_context.database_label; user; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, redirect_path)
    @@
    let%lwt view_contact_name =
      Helpers.Guard.can_read_contact_name database_label user
    in
    let* experiment = Experiment.find database_label experiment_id in
    let* session = Session.find database_label session_id in
    let* assignment = Assignment.find database_label assignment_id in
    Page.Admin.Assignment.edit
      context
      view_contact_name
      experiment
      session
      assignment
    >|> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let update req =
  let open Utils.Lwt_result.Infix in
  let open Assignment in
  let experiment_id, session_id, assignment_id = ids_from_request req in
  let redirect_path =
    Page.Admin.Assignment.assignment_specific_path
      ~suffix:"edit"
      experiment_id
      session_id
      assignment_id
  in
  let result { Pool_context.database_label; _ } =
    Utils.Lwt_result.map_error (fun err -> err, redirect_path)
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let boolean_fields = boolean_fields |> CCList.map Field.show in
    let%lwt urlencoded =
      Sihl.Web.Request.to_urlencoded req
      ||> HttpUtils.format_request_boolean_values boolean_fields
      ||> HttpUtils.remove_empty_values
    in
    let* assignment = find database_label assignment_id in
    let* experiment = Experiment.find database_label experiment_id in
    let* session = Session.find database_label session_id in
    let* participated_in_other_sessions =
      Assignment.contact_participation_in_other_assignments
        database_label
        ~exclude_assignments:[ assignment ]
        experiment_id
        (Contact.id assignment.contact)
    in
    let events =
      let open Cqrs_command.Assignment_command.Update in
      let open CCResult.Infix in
      urlencoded
      |> decode
      >>= handle
            ~tags
            experiment
            session
            assignment
            participated_in_other_sessions
      |> Lwt_result.lift
    in
    let handle events =
      let%lwt () =
        Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
      in
      Http_utils.redirect_to_with_actions
        redirect_path
        [ Message.set
            ~success:[ Pool_common.Message.(Updated Field.Assignment) ]
        ]
    in
    events |>> handle
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let remind req =
  let open Utils.Lwt_result.Infix in
  let open Assignment in
  let experiment_id, session_id, assignment_id = ids_from_request req in
  let redirect_path =
    Page.Admin.Session.session_path experiment_id session_id
  in
  let result { Pool_context.database_label; _ } =
    Utils.Lwt_result.map_error (fun err -> err, redirect_path)
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let%lwt urlencoded =
      Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
    in
    let tenant = Pool_context.Tenant.get_tenant_exn req in
    let tenant_languages = Pool_context.Tenant.get_tenant_languages_exn req in
    let* assignment = find database_label assignment_id in
    let* experiment = Experiment.find database_label experiment_id in
    let* session = Session.find database_label session_id in
    let%lwt create_messages =
      Reminder.prepare_messages
        database_label
        tenant
        tenant_languages
        experiment
        session
    in
    let events =
      let open Cqrs_command.Assignment_command.SendReminder in
      let open CCResult.Infix in
      urlencoded
      |> decode
      >>= handle ~tags create_messages experiment session assignment
      |> Lwt_result.lift
    in
    let handle events =
      let%lwt () =
        Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
      in
      Http_utils.redirect_to_with_actions
        redirect_path
        [ Message.set ~success:[ Pool_common.Message.(Sent Field.Reminder) ] ]
    in
    events |>> handle
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

module Access : sig
  include module type of Helpers.Access

  val cancel : Rock.Middleware.t
  val deleted : Rock.Middleware.t
  val mark_as_deleted : Rock.Middleware.t
end = struct
  include Helpers.Access
  module AssignmentCommand = Cqrs_command.Assignment_command
  module Guardian = Middleware.Guardian

  let experiment_effects =
    Guardian.id_effects Experiment.Id.of_string Field.Experiment
  ;;

  let combined_effects fcn req =
    let open HttpUtils in
    let experiment_id = find_id Experiment.Id.of_string Field.Experiment req in
    let assignment_id = find_id Assignment.Id.of_string Field.Assignment req in
    fcn experiment_id assignment_id
  ;;

  let index =
    Assignment.Guard.Access.index
    |> experiment_effects
    |> Guardian.validate_generic ~any_id:true
  ;;

  let delete =
    Assignment.Guard.Access.delete
    |> combined_effects
    |> Guardian.validate_generic
  ;;

  let deleted =
    Assignment.Guard.Access.deleted
    |> experiment_effects
    |> Guardian.validate_generic
  ;;

  let cancel =
    AssignmentCommand.Cancel.effects
    |> combined_effects
    |> Guardian.validate_generic
  ;;

  let mark_as_deleted =
    AssignmentCommand.MarkAsDeleted.effects
    |> combined_effects
    |> Guardian.validate_generic
  ;;

  let update =
    Assignment.Guard.Access.update
    |> combined_effects
    |> Guardian.validate_generic
  ;;
end
