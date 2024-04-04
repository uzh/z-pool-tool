module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Field = Pool_common.Message.Field
module Url = HttpUtils.Url.Admin

let src = Logs.Src.create "handler.admin.experiments_assignments"
let create_layout req = General.create_tenant_layout req
let experiment_id = HttpUtils.find_id Experiment.Id.of_string Field.Experiment
let session_id = HttpUtils.find_id Session.Id.of_string Field.Session
let assignment_id = HttpUtils.find_id Assignment.Id.of_string Field.Assignment

let ids_from_request req =
  let open Pool_common.Message.Field in
  HttpUtils.(
    ( find_id Experiment.Id.of_string Experiment req
    , find_id Session.Id.of_string Session req
    , find_id Assignment.Id.of_string Assignment req ))
;;

let cancel req =
  let open Utils.Lwt_result.Infix in
  let experiment_id, session_id, assignment_id = ids_from_request req in
  let redirect_path = Url.session_path experiment_id session_id in
  let result { Pool_context.database_label; _ } =
    Utils.Lwt_result.map_error (fun err -> err, redirect_path)
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let tenant = Pool_context.Tenant.get_tenant_exn req in
    let* experiment = Experiment.find database_label experiment_id in
    let* session = Session.find database_label session_id in
    let%lwt assignments =
      Assignment.find_with_follow_ups database_label assignment_id
    in
    let* cancellation_notification =
      let* assignment =
        CCList.find_opt
          (fun { Assignment.id; _ } -> Assignment.Id.equal id assignment_id)
          assignments
        |> CCOption.to_result Pool_common.(Message.NotFound Field.Assignment)
        |> Lwt_result.lift
      in
      let%lwt follow_up_sessions =
        Session.find_follow_ups database_label session_id
        ||> function
        | [] -> None
        | sessions -> Some sessions
      in
      Message_template.AssignmentCancellation.create
        ?follow_up_sessions
        tenant
        experiment
        session
        assignment
      |> Lwt_result.ok
    in
    let events =
      Cqrs_command.Assignment_command.Cancel.handle
        ~tags
        cancellation_notification
        (assignments, session)
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
  let experiment_id, session_id, assignment_id = ids_from_request req in
  let redirect_path = Url.session_path experiment_id session_id in
  let result { Pool_context.database_label; _ } =
    Utils.Lwt_result.map_error (fun err -> err, redirect_path)
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let%lwt assignments =
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

module Close = struct
  open Cqrs_command.Assignment_command
  open Utils.Lwt_result.Infix

  let experiment_target_id experiment_id =
    [ Guard.Uuid.target_of Experiment.Id.value experiment_id ]
  ;;

  open Assignment

  let router_params req database_label =
    let experiment_id = experiment_id req in
    let session_id = session_id req in
    let* experiment = Experiment.find database_label experiment_id in
    let* session = Session.find database_label session_id in
    Lwt_result.return (experiment, session)
  ;;

  let decode req =
    let boolean_fields = Assignment.boolean_fields |> CCList.map Field.show in
    req
    |> Sihl.Web.Request.to_urlencoded
    ||> HttpUtils.format_htmx_request_boolean_values boolean_fields
    ||> UpdateHtmx.decode
  ;;

  let updated_fields (a1 : t) (a2 : t) =
    let eq = CCOption.equal in
    [ NoShow.(eq equal a1.no_show a2.no_show, field)
    ; Participated.(eq equal a1.participated a2.participated, field)
    ; ExternalDataId.(eq equal a1.external_data_id a2.external_data_id, field)
    ]
    |> CCList.filter_map (fun (equal, field) ->
      if not equal then Some field else None)
  ;;

  let update req =
    let tags = Pool_context.Logger.Tags.req req in
    let assignment_id = assignment_id req in
    let result ({ Pool_context.database_label; language; _ } as context) =
      let* experiment, session = router_params req database_label in
      let* assignment = find database_label assignment_id in
      let* updated = decode req >|+ UpdateHtmx.handle assignment in
      let%lwt () =
        Pool_event.handle_event
          ~tags
          database_label
          (Updated updated |> Pool_event.assignment)
      in
      let%lwt counters =
        counters_of_session database_label session.Session.id
      in
      let updated_fields = updated_fields assignment updated in
      Page.Admin.Session.
        [ close_assignment_htmx_form
            ~updated_fields
            context
            experiment
            session
            updated
        ; session_counters language counters
        ]
      |> HttpUtils.Htmx.multi_html_to_plain_text_response
      |> Lwt_result.return
    in
    result
    |> HttpUtils.Htmx.handle_error_message ~error_as_notification:true ~src req
  ;;

  let toggle req =
    let tags = Pool_context.Logger.Tags.req req in
    let result ({ Pool_context.database_label; language; _ } as context) =
      let* experiment, session = router_params req database_label in
      let* decoded =
        decode req
        >== fun decoded ->
        match decoded with
        | ExternalDataId _ -> Error Pool_common.Message.InvalidHtmxRequest
        | Participated _ | NoShow _ -> Ok decoded
      in
      let%lwt assignments, custom_fields =
        find_for_session_close_screen database_label session.Session.id
      in
      let events, assignments =
        assignments
        |> CCList.fold_left
             (fun (events, assignments) original ->
               let updated = UpdateHtmx.handle original decoded in
               let updated_fields = updated_fields original updated in
               ( events @ [ Updated updated |> Pool_event.assignment ]
               , assignments @ [ updated, Some updated_fields ] ))
             ([], [])
      in
      let%lwt () = Pool_event.handle_events ~tags database_label events in
      let%lwt counters =
        counters_of_session database_label session.Session.id
      in
      let view_contact_name =
        experiment.Experiment.id
        |> experiment_target_id
        |> Helpers.Guard.can_read_contact_name context
      in
      Page.Admin.Session.
        [ close_assignments_table
            context
            view_contact_name
            experiment
            session
            assignments
            custom_fields
        ; session_counters language counters
        ]
      |> HttpUtils.Htmx.multi_html_to_plain_text_response
      |> Lwt_result.return
    in
    result
    |> HttpUtils.Htmx.handle_error_message ~error_as_notification:true ~src req
  ;;
end

let edit req =
  let open Utils.Lwt_result.Infix in
  let experiment_id, session_id, assignment_id = ids_from_request req in
  let redirect_path = Url.session_path experiment_id session_id in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, redirect_path)
    @@
    let view_contact_name =
      Helpers.Guard.can_read_contact_name
        context
        [ Guard.Uuid.target_of Experiment.Id.value experiment_id ]
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
      >>= handle ~tags create_messages session assignment
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

let swap_session_get_helper action req =
  let open Assignment in
  let experiment_id, session_id, assignment_id = ids_from_request req in
  let result ({ Pool_context.database_label; _ } as context) =
    let open Utils.Lwt_result.Infix in
    let* experiment = Experiment.find database_label experiment_id in
    let* assignment = find database_label assignment_id in
    let* template_lang =
      match action with
      | `OpenModal ->
        let system_languages =
          Pool_context.Tenant.get_tenant_languages_exn req
        in
        Message_template.experiment_message_language
          system_languages
          experiment
          assignment.contact
        |> Lwt_result.return
      | `ToggleLanguage ->
        HttpUtils.find_query_param
          req
          Field.Language
          Pool_common.Language.create
        |> Lwt_result.lift
    in
    let* swap_session_template =
      Message_template.(
        find_entity_defaults_by_label
          ~entity_uuids:
            [ Session.Id.to_common session_id
            ; Experiment.Id.to_common experiment_id
            ]
          database_label
          [ template_lang ]
          Label.AssignmentSessionChange)
      ||> CCList.head_opt
      ||> CCOption.to_result Pool_common.Message.(NotFound Field.Template)
    in
    let text_messages_disabled =
      Pool_context.Tenant.text_messages_enabled req
    in
    let tenant_languages = Pool_context.Tenant.get_tenant_languages_exn req in
    let flash_fetcher key = Sihl.Web.Flash.find key req in
    let response html =
      html |> HttpUtils.Htmx.html_to_plain_text_response |> Lwt_result.return
    in
    match action with
    | `OpenModal ->
      let* current_session = Session.find database_label session_id in
      let%lwt assigned_sessions =
        Session.find_contact_is_assigned_by_experiment
          database_label
          (Contact.id assignment.contact)
          experiment_id
      in
      let%lwt sessions =
        Session.find_all_to_swap_by_experiment database_label experiment_id
      in
      Page.Admin.Assignment.Partials.swap_session_form
        context
        experiment
        current_session
        assignment
        assigned_sessions
        sessions
        swap_session_template
        tenant_languages
        flash_fetcher
        text_messages_disabled
      |> response
    | `ToggleLanguage ->
      Page.Admin.Assignment.Partials.swap_session_notification_form_fields
        context
        experiment
        session_id
        assignment_id
        tenant_languages
        swap_session_template
        flash_fetcher
        text_messages_disabled
      |> response
  in
  result
  |> HttpUtils.Htmx.handle_error_message ~error_as_notification:true ~src req
;;

let swap_session_get = swap_session_get_helper `OpenModal
let swap_session_toggle_language = swap_session_get_helper `ToggleLanguage

let swap_session_post req =
  let open Utils.Lwt_result.Infix in
  let open Assignment in
  let open Cqrs_command.Assignment_command in
  let experiment_id, session_id, assignment_id = ids_from_request req in
  let redirect_path =
    Page.Admin.Session.session_path experiment_id session_id
  in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req
    ||> HttpUtils.remove_empty_values
    ||> HttpUtils.format_request_boolean_values Field.[ show NotifyContact ]
  in
  let result { Pool_context.database_label; _ } =
    Utils.Lwt_result.map_error (fun err ->
      err, redirect_path, [ HttpUtils.urlencoded_to_flash urlencoded ])
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let* decoded = SwapSession.decode urlencoded |> Lwt_result.lift in
    let* assignment = find database_label assignment_id in
    let* experiment = Experiment.find database_label experiment_id in
    let* current_session = Session.find database_label session_id in
    let* new_session = Session.find database_label decoded.session in
    let%lwt notification_email =
      match decoded.notify_contact |> Pool_common.NotifyContact.value with
      | false -> Lwt.return_none
      | true ->
        let tenant = Pool_context.Tenant.get_tenant_exn req in
        let msg =
          Message_template.ManualMessage.
            { recipient = Contact.email_address assignment.contact
            ; language = decoded.language
            ; email_subject = decoded.email_subject
            ; email_text = decoded.email_text
            ; plain_text = decoded.plain_text
            }
        in
        Message_template.AssignmentSessionChange.create
          msg
          tenant
          experiment
          ~new_session
          ~old_session:current_session
          assignment
        ||> CCOption.return
    in
    let events =
      SwapSession.handle
        ~tags
        ~current_session
        ~new_session
        assignment
        notification_email
      |> Lwt_result.lift
    in
    let handle events =
      let%lwt () =
        Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
      in
      Http_utils.redirect_to_with_actions
        redirect_path
        [ Message.set ~success:[ Pool_common.Message.(Updated Field.Session) ] ]
    in
    events |>> handle
  in
  result |> HttpUtils.extract_happy_path_with_actions ~src req
;;

module Access : sig
  include module type of Helpers.Access

  val cancel : Rock.Middleware.t
  val mark_as_deleted : Rock.Middleware.t
end = struct
  include Helpers.Access
  module AssignmentCommand = Cqrs_command.Assignment_command
  module Guardian = Middleware.Guardian

  let combined_effects fcn req =
    let open HttpUtils in
    let experiment_id = find_id Experiment.Id.of_string Field.Experiment req in
    let assignment_id = find_id Assignment.Id.of_string Field.Assignment req in
    fcn experiment_id assignment_id
  ;;

  let delete =
    Assignment.Guard.Access.delete
    |> combined_effects
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
