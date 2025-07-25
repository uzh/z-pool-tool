open CCFun
open Pool_message
open Utils.Lwt_result.Infix
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Response = Http_response

let src = Logs.Src.create "handler.admin.session"
let create_layout req = General.create_tenant_layout req
let experiment_id = HttpUtils.find_id Experiment.Id.of_string Field.Experiment
let session_id = HttpUtils.find_id Session.Id.of_string Field.Session
let template_id = HttpUtils.find_id Message_template.Id.of_string Field.MessageTemplate
let session_path = HttpUtils.Url.Admin.session_path

let location urlencoded database_label =
  Field.(Location |> show)
  |> CCList.pure
  |> HttpUtils.urlencoded_to_params urlencoded
  |> CCOption.to_result (Error.NotFound Field.Location)
  |> Lwt_result.lift
  >|+ List.assoc Field.(Location |> show)
  >|+ Pool_location.Id.of_string
  >>= Pool_location.find database_label
;;

let default_lead_time_settings database_label =
  Lwt.both
    (Settings.find_default_reminder_lead_time database_label)
    (Settings.find_default_text_msg_reminder_lead_time database_label)
;;

let can_access_session_assistants pool actor experiment_id =
  let id = experiment_id |> Guard.Uuid.target_of Experiment.Id.value in
  let role = `Assistant, Some id in
  Admin_experiments_users.has_permission_on_role pool actor role Guard.Permission.Read
;;

let can_update_session { Pool_context.guardian; _ } session_id =
  Guard.PermissionOnTarget.validate
    (Session.Guard.Access.update_permission_on_target session_id)
    guardian
;;

let list req =
  let experiment_id = experiment_id req in
  Response.Htmx.index_handler ~create_layout ~query:(module Session) req
  @@ fun ({ Pool_context.database_label; user; _ } as context) query ->
  let* experiment = Experiment.find database_label experiment_id in
  let flatten_sessions =
    CCList.fold_left (fun acc (parent, follow_ups) -> acc @ (parent :: follow_ups)) []
  in
  let* actor = Pool_context.Utils.find_authorizable database_label user in
  let%lwt can_access_session_assistants =
    can_access_session_assistants database_label actor experiment_id
  in
  Lwt_result.ok
  @@
  match Experiment.is_sessionless experiment with
  | true ->
    let to_html time_windows =
      let open Page.Admin.TimeWindow in
      match HttpUtils.Htmx.is_hx_request req with
      | true ->
        data_table ~can_access_session_assistants context experiment time_windows
        |> Lwt.return
      | false -> index ~can_access_session_assistants context experiment time_windows
    in
    Time_window.query_by_experiment ~query database_label experiment_id >|> to_html
  | false ->
    let chronological =
      Sihl.Web.Request.query Field.(show Chronological) req
      |> CCOption.map_or ~default:false (CCString.equal "true")
    in
    let to_html sessions =
      let open Page.Admin.Session in
      match HttpUtils.Htmx.is_hx_request req with
      | true ->
        data_table
          context
          ~can_access_session_assistants
          experiment
          sessions
          chronological
        |> Lwt.return
      | false ->
        index context ~can_access_session_assistants experiment sessions chronological
    in
    (match chronological with
     | true -> Session.query_by_experiment ~query database_label experiment_id >|> to_html
     | false ->
       Session.query_grouped_by_experiment ~query database_label experiment_id
       ||> (fun (sessions, query) -> flatten_sessions sessions, query)
       >|> to_html)
;;

let new_helper req page =
  let open Session in
  let id = experiment_id req in
  let result ({ Pool_context.database_label; _ } as context) =
    Response.bad_request_render_error context
    @@ let* experiment = Experiment.find database_label id in
       let%lwt locations = Pool_location.all database_label in
       let%lwt default_leadtime_settings = default_lead_time_settings database_label in
       let%lwt text_messages_enabled = Pool_context.Tenant.text_messages_enabled req in
       let html =
         match page with
         | `FollowUp ->
           let session_id = session_id req in
           let* parent_session = find database_label session_id in
           let* () =
             match parent_session.follow_up_to with
             | Some _ -> Lwt_result.fail Error.SessionIsFollowup
             | None -> Lwt_result.return ()
           in
           Page.Admin.Session.follow_up
             context
             experiment
             default_leadtime_settings
             parent_session
             locations
             text_messages_enabled
           |> Lwt_result.ok
         | `New ->
           Lwt_result.ok
           @@
             (match CCOption.is_some experiment.Experiment.online_experiment with
             | false ->
               Page.Admin.Session.new_form
                 context
                 experiment
                 default_leadtime_settings
                 locations
                 text_messages_enabled
             | true -> Page.Admin.TimeWindow.new_form context experiment)
       in
       html >>= create_layout req context >|+ Sihl.Web.Response.of_html
  in
  Response.handle ~src req result
;;

let new_form req = new_helper req `New
let follow_up req = new_helper req `FollowUp

let duplication_session_data req database_label =
  let open Session in
  let experiment_id = experiment_id req in
  let session_id = session_id req in
  let* experiment = Experiment.find database_label experiment_id in
  let* session = find database_label session_id in
  let* parent_session =
    session.follow_up_to
    |> CCOption.map_or ~default:(Lwt_result.return None) (fun id ->
      find database_label id >|+ CCOption.return)
  in
  let%lwt followups = find_follow_ups database_label session_id in
  Lwt_result.return (experiment, session, followups, parent_session)
;;

let duplicate req =
  let result ({ Pool_context.database_label; _ } as context) =
    Response.bad_request_render_error context
    @@ let* experiment, session, followups, parent_session =
         duplication_session_data req database_label
       in
       Page.Admin.Session.duplicate context experiment session ?parent_session followups
       >|> create_layout req context
       >|+ Sihl.Web.Response.of_html
  in
  Response.handle ~src req result
;;

let duplicate_form_htmx req =
  let result { Pool_context.language; database_label; _ } =
    let* _, session, followups, parent_session =
      duplication_session_data req database_label
    in
    let* counter =
      Sihl.Web.Request.query "counter" req
      |> CCFun.flip CCOption.bind CCInt.of_string
      |> CCOption.to_result Error.InvalidHtmxRequest
      |> Lwt_result.lift
      >|+ ( + ) 1
    in
    Page.Admin.Session.duplicate_form ?parent_session language session followups counter
    |> Response.Htmx.of_html
    |> Lwt_result.return
  in
  Response.Htmx.handle ~src req result
;;

let duplicate_post_htmx req =
  let experiment_id = experiment_id req in
  let result { Pool_context.database_label; user; _ } =
    let tags = Pool_context.Logger.Tags.req req in
    let%lwt urlencoded =
      Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
    in
    let* _, session, followups, parent_session =
      duplication_session_data req database_label
    in
    let* events =
      let open Cqrs_command.Session_command.Duplicate in
      urlencoded |> handle ~tags ?parent_session session followups |> Lwt_result.lift
    in
    let%lwt () = Pool_event.handle_events ~tags database_label user events in
    Response.Htmx.redirect
      ~actions:[ Message.set ~success:[ Success.Created Field.Sessions ] ]
      (session_path experiment_id)
    |> Lwt_result.ok
  in
  Response.Htmx.handle ~error_as_notification:true ~src req result
;;

let create req =
  let id = experiment_id req in
  let result { Pool_context.database_label; user; _ } =
    let tags = Pool_context.Logger.Tags.req req in
    let%lwt urlencoded =
      Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
    in
    Response.bad_request_on_error ~urlencoded new_form
    @@
    let* experiment = Experiment.find database_label id in
    let field =
      if Experiment.is_sessionless experiment then Field.TimeWindow else Field.Session
    in
    let open Cqrs_command.Session_command in
    let* events =
      match Experiment.is_sessionless experiment with
      | true ->
        let open CreateTimeWindow in
        let open Time_window in
        let* decoded = urlencoded |> decode |> Lwt_result.lift in
        let%lwt overlapps =
          Time_window.find_overlapping
            database_label
            experiment.Experiment.id
            ~start:decoded.start
            ~end_at:decoded.end_at
        in
        handle ~tags ~overlapps experiment decoded |> Lwt_result.lift
      | false ->
        let* location = location urlencoded database_label in
        let open CCResult.Infix in
        let open Create in
        urlencoded |> decode >>= handle ~tags experiment location |> Lwt_result.lift
    in
    let%lwt () = Pool_event.handle_events ~tags database_label user events in
    Http_utils.redirect_to_with_actions
      (session_path id)
      [ Message.set ~success:[ Success.Created field ] ]
    |> Lwt_result.ok
  in
  Response.handle ~src req result
;;

let session_page database_label req context session experiment =
  let open Utils.Lwt_result.Infix in
  let open Helpers.Guard in
  let session_id = Session.(session.id) in
  let experiment_id = Experiment.(experiment.id) in
  let experiment_target_id = [ Guard.Uuid.target_of Experiment.Id.value experiment_id ] in
  let view_contact_name = can_read_contact_name context experiment_target_id in
  let view_contact_info = can_read_contact_info context experiment_target_id in
  let current_tags () =
    let open Tags.ParticipationTags in
    find_all database_label (Session (Session.Id.to_common session_id))
  in
  let create_layout = create_layout req context in
  function
  | `Edit ->
    let%lwt current_tags = current_tags () in
    let%lwt locations = Pool_location.all database_label in
    let%lwt default_leadtime_settings = default_lead_time_settings database_label in
    let%lwt available_tags =
      Tags.ParticipationTags.(
        find_available database_label (Session (Session.Id.to_common session_id)))
    in
    let%lwt experiment_participation_tags =
      Tags.ParticipationTags.(
        find_all database_label (Experiment (Experiment.Id.to_common experiment_id)))
    in
    let%lwt text_messages_enabled = Pool_context.Tenant.text_messages_enabled req in
    Page.Admin.Session.edit
      context
      experiment
      default_leadtime_settings
      session
      locations
      (current_tags, available_tags, experiment_participation_tags)
      text_messages_enabled
    >|> create_layout
  | `Close ->
    let%lwt assignments, custom_fields =
      Assignment.find_for_session_close_screen database_label session.Session.id
    in
    let%lwt participation_tags =
      Tags.ParticipationTags.(
        find_all database_label (Experiment (Experiment.Id.to_common experiment_id)))
    in
    let%lwt counters = Assignment.counters_of_session database_label session_id in
    Page.Admin.Session.close
      ~view_contact_name
      context
      experiment
      session
      assignments
      custom_fields
      participation_tags
      counters
    >|> create_layout
  | `Reschedule ->
    let* experiment = Experiment.find database_label experiment_id in
    Page.Admin.Session.reschedule_session context experiment session >|> create_layout
  | `Cancel ->
    let%lwt follow_ups = Session.find_follow_ups database_label session_id in
    Page.Admin.Session.cancel context experiment session follow_ups >|> create_layout
  | `Print ->
    let%lwt assignments =
      Assignment.(
        find_for_session_detail_screen ~query:default_query database_label session_id)
    in
    Page.Admin.Session.print
      ~view_contact_name
      ~view_contact_info
      context
      (`Session session)
      assignments
    |> Lwt_result.return
;;

let time_window_page database_label req context time_window experiment =
  let open Utils.Lwt_result.Infix in
  let open Helpers.Guard in
  let time_window_id = time_window.Time_window.id in
  let experiment_id = Experiment.(experiment.id) in
  let experiment_target_id = [ Guard.Uuid.target_of Experiment.Id.value experiment_id ] in
  let view_contact_name = can_read_contact_name context experiment_target_id in
  let view_contact_info = can_read_contact_info context experiment_target_id in
  let current_tags () =
    let open Tags.ParticipationTags in
    find_all database_label (Session (Session.Id.to_common time_window_id))
  in
  let create_layout = create_layout req context in
  function
  | `Edit ->
    let%lwt current_tags = current_tags () in
    let%lwt available_tags =
      Tags.ParticipationTags.(
        find_available database_label (Session (Session.Id.to_common time_window_id)))
    in
    let%lwt experiment_participation_tags =
      Tags.ParticipationTags.(
        find_all database_label (Experiment (Experiment.Id.to_common experiment_id)))
    in
    let tags = current_tags, available_tags, experiment_participation_tags in
    Page.Admin.TimeWindow.edit context experiment time_window tags >|> create_layout
  | `Print ->
    let%lwt assignments =
      Assignment.(
        find_for_session_detail_screen ~query:default_query database_label time_window_id)
    in
    Page.Admin.Session.print
      ~view_contact_name
      ~view_contact_info
      context
      (`TimeWindow time_window)
      assignments
    |> Lwt_result.return
;;

let show req =
  let open Helpers.Guard in
  let experiment_id = experiment_id req in
  let session_id = session_id req in
  let experiment_target_id = [ Guard.Uuid.target_of Experiment.Id.value experiment_id ] in
  Response.Htmx.index_handler ~create_layout ~query:(module Assignment) req
  @@ fun ({ Pool_context.database_label; user; _ } as context) query ->
  let open Utils.Lwt_result.Infix in
  let* experiment = Experiment.find database_label experiment_id in
  let* session =
    match Experiment.is_sessionless experiment with
    | true ->
      Time_window.find database_label session_id
      >|+ fun time_window -> `TimeWindow time_window
    | false -> Session.find database_label session_id >|+ fun session -> `Session session
  in
  let view_contact_name = can_read_contact_name context experiment_target_id in
  let view_contact_info = can_read_contact_info context experiment_target_id in
  let access_contact_profiles = can_access_contact_profile context experiment_id in
  let can_update_session = can_update_session context session_id in
  let%lwt text_messages_enabled = Pool_context.Tenant.text_messages_enabled req in
  let%lwt assignments =
    Assignment.find_for_session_detail_screen ~query database_label session_id
  in
  match HttpUtils.Htmx.is_hx_request req with
  | false ->
    let%lwt not_matching_filter_count =
      match HttpUtils.Session.canceled_at session with
      | Some _ -> Lwt.return_none
      | None ->
        Assignment.count_unsuitable_by database_label (`Session session_id)
        ||> CCOption.return
    in
    let%lwt current_tags =
      Tags.ParticipationTags.(
        find_all database_label (Session (Session.Id.to_common session_id)))
    in
    let%lwt session_reminder_templates =
      Message_template.find_all_of_entity_by_label
        database_label
        (session_id |> Session.Id.to_common)
        Message_template.Label.SessionReminder
    in
    let sys_languages = Pool_context.Tenant.get_tenant_languages_exn req in
    let%lwt send_direct_message =
      Helpers.Guard.can_send_direct_message experiment_id context
    in
    let%lwt rerun_session_filter =
      Helpers.Guard.can_rerun_session_filter context experiment_id session_id
    in
    let* actor = Pool_context.Utils.find_authorizable database_label user in
    let%lwt can_access_session_assistants =
      can_access_session_assistants database_label actor experiment_id
    in
    (match session with
     | `Session session ->
       Page.Admin.Session.detail
         ~access_contact_profiles
         ~can_access_session_assistants
         ~can_update_session
         ~not_matching_filter_count
         ~rerun_session_filter
         ~send_direct_message
         ~view_contact_name
         ~view_contact_info
         context
         experiment
         session
         current_tags
         sys_languages
         session_reminder_templates
         text_messages_enabled
         assignments
       |> Lwt_result.ok
     | `TimeWindow time_window ->
       Page.Admin.TimeWindow.detail
         ~access_contact_profiles
         ~can_access_session_assistants
         ~send_direct_message
         ~view_contact_name
         ~view_contact_info
         ~text_messages_enabled
         context
         experiment
         time_window
         current_tags
         assignments
       |> Lwt_result.ok)
  | true ->
    Page.Admin.Assignment.(
      data_table
        ~access_contact_profiles
        ~view_contact_name
        ~view_contact_info
        context
        session
        text_messages_enabled
        assignments)
    |> Lwt_result.return
;;

let detail page req =
  let open Utils.Lwt_result.Infix in
  let session_id = session_id req in
  let result ({ Pool_context.database_label; _ } as context) =
    Response.bad_request_render_error context
    @@
    let* experiment =
      Experiment.find_of_session database_label (session_id |> Session.Id.to_common)
    in
    (match Experiment.is_sessionless experiment with
     | false ->
       let* session = Session.find database_label session_id in
       session_page database_label req context session experiment page
     | true ->
       let* time_window = Time_window.find database_label session_id in
       let* page =
         match page with
         | `Edit -> Lwt_result.return `Edit
         | `Print -> Lwt_result.return `Print
         | `Reschedule | `Cancel | `Close -> Lwt_result.fail (Error.Invalid Field.Action)
       in
       time_window_page database_label req context time_window experiment page)
    >|+ Sihl.Web.Response.of_html
  in
  Response.handle ~src req result
;;

let edit = detail `Edit
let reschedule_form = detail `Reschedule
let cancel_form = detail `Cancel
let close = detail `Close
let print = detail `Print

let update_handler action req =
  let experiment_id = experiment_id req in
  let session_id = session_id req in
  let path = session_path experiment_id ~id:session_id in
  let error_handler =
    match action with
    | `Update -> edit
    | `Reschedule -> reschedule_form
  in
  let result { Pool_context.database_label; user; _ } =
    let* session = Session.find database_label session_id >|- Response.not_found in
    let%lwt urlencoded =
      Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
    in
    Response.bad_request_on_error ~urlencoded error_handler
    @@
    let sessions_data () =
      let%lwt follow_ups = Session.find_follow_ups database_label session.Session.id in
      let* parent =
        match session.Session.follow_up_to with
        | None -> Lwt_result.return None
        | Some parent_id -> parent_id |> Session.find database_label >|+ CCOption.return
      in
      Lwt_result.return (session, follow_ups, parent)
    in
    let tags = Pool_context.Logger.Tags.req req in
    let tenant = Pool_context.Tenant.get_tenant_exn req in
    let* experiment = Experiment.find database_label experiment_id in
    let field =
      if Experiment.is_sessionless experiment then Field.TimeWindow else Field.Session
    in
    let success_msg =
      match action with
      | `Update -> Success.Updated field
      | `Reschedule -> Success.Rescheduled field
    in
    let open Cqrs_command.Session_command in
    let* events =
      match action with
      | `Update ->
        (match Experiment.is_sessionless experiment with
         | true ->
           let open UpdateTimeWindow in
           let* time_window = Time_window.find database_label session_id in
           let* decoded = urlencoded |> decode |> Lwt_result.lift in
           let%lwt overlapps =
             let open Time_window in
             find_overlapping
               ~exclude:session_id
               database_label
               experiment.Experiment.id
               ~start:decoded.start
               ~end_at:decoded.end_at
           in
           handle ~tags ~overlapps time_window decoded |> Lwt_result.lift
         | false ->
           let* session, follow_ups, parent = sessions_data () in
           let* location = location urlencoded database_label in
           let open CCResult.Infix in
           let open Update in
           urlencoded
           |> decode
           >>= handle ~tags ?parent_session:parent follow_ups session location
           |> Lwt_result.lift)
      | `Reschedule ->
        let open Reschedule in
        let* session, follow_ups, parent = sessions_data () in
        let%lwt assignments =
          Assignment.find_uncanceled_by_session database_label session.Session.id
        in
        let system_languages = Pool_context.Tenant.get_tenant_languages_exn req in
        let%lwt create_message =
          Message_template.SessionReschedule.prepare
            database_label
            tenant
            experiment
            system_languages
            session
        in
        urlencoded
        |> decode
        |> Lwt_result.lift
        >== handle
              ~tags
              ?parent_session:parent
              follow_ups
              session
              assignments
              create_message
    in
    let%lwt () = Pool_event.handle_events ~tags database_label user events in
    Http_utils.redirect_to_with_actions path [ Message.set ~success:[ success_msg ] ]
    |> Lwt_result.ok
  in
  Response.handle ~src req result
;;

let update = update_handler `Update
let reschedule = update_handler `Reschedule

let cancel req =
  let open Utils.Lwt_result.Infix in
  let experiment_id = experiment_id req in
  let session_id = session_id req in
  let success_path = session_path experiment_id ~id:session_id in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req
    ||> HttpUtils.remove_empty_values
    ||> HttpUtils.format_request_boolean_values Field.[ show Email; show SMS ]
  in
  let result { Pool_context.database_label; user; _ } =
    let* session = Session.find database_label session_id >|- Response.not_found in
    Response.bad_request_on_error ~urlencoded cancel_form
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let* experiment = Experiment.find database_label experiment_id in
    let%lwt follow_ups = Session.find_follow_ups database_label session.Session.id in
    let%lwt assignments =
      session :: follow_ups
      |> Lwt_list.fold_left_s
           (fun assignments session ->
              Assignment.find_uncanceled_by_session database_label session.Session.id
              ||> CCList.append assignments)
           []
      ||> Assignment.group_by_contact
    in
    let* notify_via =
      let open Pool_common in
      Sihl.Web.Request.urlencoded_list Field.(NotifyVia |> array_key) req
      ||> CCList.map NotifyVia.create
      ||> CCList.all_ok
      >|+ CCList.uniq ~eq:NotifyVia.equal
      >>= function
      | [] -> Lwt_result.fail (Error.NoOptionSelected Field.NotifyVia)
      | notify_via -> Lwt_result.return notify_via
    in
    let* events =
      let system_languages = Pool_context.Tenant.get_tenant_languages_exn req in
      let tenant = Pool_context.Tenant.get_tenant_exn req in
      let%lwt create_email =
        Message_template.SessionCancellation.prepare
          database_label
          tenant
          experiment
          system_languages
          session
          follow_ups
      in
      let%lwt create_text_message =
        Message_template.SessionCancellation.prepare_text_message
          database_label
          tenant
          experiment
          system_languages
          session
          follow_ups
      in
      let open CCResult.Infix in
      let open Cqrs_command.Session_command.Cancel in
      urlencoded
      |> decode
      >>= handle
            ~tags
            (session :: follow_ups)
            assignments
            create_email
            create_text_message
            notify_via
      |> Lwt_result.lift
    in
    let%lwt () = Pool_event.handle_events ~tags database_label user events in
    Http_utils.redirect_to_with_actions
      success_path
      [ Message.set ~success:[ Success.Canceled Field.Session ] ]
    |> Lwt_result.ok
  in
  Response.handle ~src req result
;;

let delete req =
  let experiment_id = experiment_id req in
  let result { Pool_context.database_label; user; _ } =
    let session_id = session_id req in
    let* session = Session.find database_label session_id >|- Response.not_found in
    Response.bad_request_on_error list
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let%lwt follow_ups = Session.find_follow_ups database_label session_id in
    let%lwt templates =
      let open Message_template in
      find_all_of_entity_by_label
        database_label
        (session_id |> Session.Id.to_common)
        Label.SessionReminder
    in
    let* events =
      let open Cqrs_command.Session_command.Delete in
      { session; follow_ups; templates } |> handle ~tags |> Lwt_result.lift
    in
    let%lwt () = Pool_event.handle_events ~tags database_label user events in
    Http_utils.redirect_to_with_actions
      (session_path experiment_id)
      [ Message.set ~success:[ Success.Deleted Field.Session ] ]
    |> Lwt_result.ok
  in
  Response.handle ~src req result
;;

(* TODO [aerben] make possible to create multiple follow ups? *)
let create_follow_up req =
  let experiment_id = experiment_id req in
  let session_id = session_id req in
  let result { Pool_context.database_label; user; _ } =
    let* session = Session.find database_label session_id >|- Response.not_found in
    let%lwt urlencoded =
      Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
    in
    Response.bad_request_on_error ~urlencoded follow_up
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let* location = location urlencoded database_label in
    let* experiment = Experiment.find database_label experiment_id in
    let* events =
      let open CCResult.Infix in
      let open Cqrs_command.Session_command.Create in
      urlencoded
      |> decode
      >>= handle ~tags ~parent_session:session experiment location
      |> Lwt_result.lift
    in
    let%lwt () = Pool_event.handle_events ~tags database_label user events in
    Http_utils.redirect_to_with_actions
      (session_path experiment_id)
      [ Message.set ~success:[ Success.Created Field.Session ] ]
    |> Lwt_result.ok
  in
  Response.handle ~src req result
;;

let close_post req =
  let experiment_id = experiment_id req in
  let session_id = session_id req in
  let path = session_path experiment_id ~id:session_id in
  let result { Pool_context.database_label; user; _ } =
    let* session = Session.find database_label session_id >|- Response.not_found in
    Response.bad_request_on_error close
    @@
    let open Cqrs_command.Session_command in
    let* experiment = Experiment.find database_label experiment_id in
    let%lwt assignments =
      Assignment.find_uncanceled_by_session database_label session.Session.id
    in
    let%lwt participation_tags =
      let open Tags.ParticipationTags in
      [ Experiment (Experiment.Id.to_common experiment_id)
      ; Session (Session.Id.to_common session_id)
      ]
      |> Lwt_list.fold_left_s
           (fun tags entity -> find_all database_label entity ||> CCList.append tags)
           []
    in
    let* events =
      assignments
      |> Lwt_list.map_s
           (fun ({ Assignment.no_show; participated; contact; _ } as assignment) ->
              let open Assignment in
              let* increment_num_participations =
                Assignment.contact_participation_in_other_assignments
                  database_label
                  ~exclude_assignments:[ assignment ]
                  experiment_id
                  (Contact.id contact)
                >|+ not %> IncrementParticipationCount.create
              in
              let%lwt follow_ups =
                let with_default fnc = CCOption.map_or ~default:false fnc in
                match
                  with_default NoShow.value no_show
                  || not (with_default Participated.value participated)
                with
                | true -> find_follow_ups database_label assignment ||> CCOption.return
                | false -> Lwt.return_none
              in
              Lwt_result.return (assignment, increment_num_participations, follow_ups))
      ||> CCResult.flatten_l
      >== Close.handle experiment session participation_tags
    in
    let%lwt () = Pool_event.handle_events database_label user events in
    Http_utils.redirect_to_with_actions
      path
      [ Message.set ~success:[ Success.Closed Field.Session ] ]
    |> Lwt_result.ok
  in
  Response.handle ~src req result
;;

let message_template_form ?template_id label req =
  let open Utils.Lwt_result.Infix in
  let experiment_id = experiment_id req in
  let session_id = session_id req in
  let result ({ Pool_context.database_label; _ } as context) =
    let* experiment =
      Experiment.find database_label experiment_id >|- Response.not_found
    in
    Response.bad_request_render_error context
    @@
    let open Message_template in
    let tenant = Pool_context.Tenant.get_tenant_exn req in
    let* session = Session.find database_label session_id in
    let%lwt text_messages_enabled = Pool_context.Tenant.text_messages_enabled req in
    let* form_context, available_languages =
      match template_id with
      | None ->
        let%lwt languages =
          Pool_context.Tenant.get_tenant_languages_exn req
          |> missing_template_languages
               database_label
               (session_id |> Session.Id.to_common)
               label
        in
        let%lwt template =
          find_entity_defaults_by_label
            database_label
            ~entity_uuids:
              [ Experiment.Id.to_common experiment_id; Session.Id.to_common session_id ]
            languages
            label
          ||> CCList.hd
        in
        Lwt_result.return (`Create template, Some languages)
      | Some template_id ->
        let* template = Message_template.find database_label template_id in
        Lwt_result.return (`Update template, None)
    in
    Page.Admin.Session.message_template_form
      ~text_messages_enabled
      context
      tenant
      experiment
      session
      available_languages
      label
      form_context
    >|> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  Response.handle ~src req result
;;

let new_session_reminder req =
  message_template_form Message_template.Label.SessionReminder req
;;

let new_session_reminder_post req =
  let open Admin_message_templates in
  let experiment_id = experiment_id req in
  let session_id = session_id req in
  let entity_id = session_id |> Session.Id.to_common in
  let label = Message_template.Label.SessionReminder in
  let redirect =
    { success = session_path ~id:session_id experiment_id; error = new_session_reminder }
  in
  (write (Create (entity_id, label, redirect))) req
;;

let edit_template req =
  let template_id = template_id req in
  message_template_form ~template_id Message_template.Label.SessionReminder req
;;

let update_template req =
  let open Admin_message_templates in
  let experiment_id = experiment_id req in
  let session_id = session_id req in
  let template_id = template_id req in
  let redirect =
    { success = session_path ~id:session_id ~suffix:"edit" experiment_id
    ; error = edit_template
    }
  in
  (write (Update (template_id, redirect))) req
;;

let message_template_changelog req =
  let open Message_template in
  let experiment_id = experiment_id req in
  let session_id = session_id req in
  let id = template_id req in
  let label = Message_template.Label.SessionReminder in
  let url =
    HttpUtils.Url.Admin.session_message_template_path
      experiment_id
      session_id
      label
      ~suffix:"changelog"
      ~id
      ()
  in
  Helpers.Changelog.htmx_handler ~url (Id.to_common id) req
;;

(* TODO: Where to find the trigger of this handler? *)
let delete_message_template req =
  let result { Pool_context.database_label; user; _ } =
    Response.bad_request_on_error show
    @@
    let experiment_id = experiment_id req in
    let session_id = session_id req in
    let template_id = template_id req in
    let redirect = session_path experiment_id ~id:session_id in
    Helpers.MessageTemplates.delete database_label user template_id redirect
  in
  Response.handle ~src req result
;;

let resend_reminders req =
  let experiment_id = experiment_id req in
  let session_id = session_id req in
  let tags = Pool_context.Logger.Tags.req req in
  let result { Pool_context.database_label; user; _ } =
    let%lwt urlencoded =
      Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
    in
    let* experiment =
      Experiment.find database_label experiment_id >|- Response.not_found
    in
    let* session = Session.find database_label session_id >|- Response.not_found in
    Response.bad_request_on_error ~urlencoded show
    @@
    let%lwt assignments =
      Assignment.find_uncanceled_by_session database_label session.Session.id
    in
    let tenant = Pool_context.Tenant.get_tenant_exn req in
    let tenant_languages = Pool_context.Tenant.get_tenant_languages_exn req in
    let%lwt create_messages =
      Reminder.prepare_messages database_label tenant tenant_languages experiment session
    in
    let* events =
      let open CCResult.Infix in
      let open Cqrs_command.Session_command.ResendReminders in
      urlencoded
      |> decode
      >>= handle ~tags create_messages session assignments
      |> Lwt_result.lift
    in
    let%lwt () = Pool_event.handle_events ~tags database_label user events in
    Http_utils.redirect_to_with_actions
      (session_path ~id:session_id experiment_id)
      [ Message.set ~success:[ Success.RemindersResent ] ]
    |> Lwt_result.ok
  in
  Response.handle ~src req result
;;

let changelog req =
  let open Session in
  let experiment_id = experiment_id req in
  let id = session_id req in
  let url = HttpUtils.Url.Admin.session_path ~suffix:"changelog" ~id experiment_id in
  Helpers.Changelog.htmx_handler ~url (Id.to_common id) req
;;

module DirectMessage = struct
  let assignments_from_requeset req database_label session_id =
    let open Assignment in
    Sihl.Web.Request.urlencoded_list Field.(array_key Assignment) req
    ||> CCList.map Id.of_string
    >|> find_multiple_by_session database_label session_id
    ||> function
    | [] -> Error (Error.NoOptionSelected Field.Assignment)
    | [ assignment ] -> Ok (`One assignment)
    | assignments -> Ok (`Multiple assignments)
  ;;

  let message_channel urlencoded =
    let open CCOption in
    CCList.assoc_opt ~eq:( = ) Field.(show MessageChannel) urlencoded
    >>= CCList.head_opt
    |> function
    | None -> Error (Error.Missing Field.MessageChannel)
    | Some channel -> Pool_common.MessageChannel.create channel
  ;;

  let to_assignment_list = function
    | `One assignment -> [ assignment ]
    | `Multiple assignments -> assignments
  ;;

  let modal_htmx req =
    let session_id = session_id req in
    let result ({ Pool_context.database_label; _ } as context) =
      let* assignments = assignments_from_requeset req database_label session_id in
      let* session = Session.find database_label session_id in
      let system_languages = Pool_context.Tenant.get_tenant_languages_exn req in
      let%lwt message_template =
        let language =
          session.Session.experiment.Experiment.language
          |> CCOption.value ~default:(CCList.hd system_languages)
        in
        Message_template.(
          find_by_label_and_language_to_send
            database_label
            Label.ManualSessionMessage
            language)
      in
      Page.Admin.Assignment.Partials.direct_message_modal
        context
        session
        message_template
        system_languages
        assignments
      |> Response.Htmx.of_html
      |> Lwt_result.return
    in
    Response.Htmx.handle ~error_as_notification:true ~src req result
  ;;

  let send req =
    let tags = Pool_context.Logger.Tags.req req in
    let experiment_id = experiment_id req in
    let session_id = session_id req in
    let session_path = session_path experiment_id ~id:session_id in
    let result { Pool_context.database_label; user; _ } =
      let%lwt urlencoded =
        Sihl.Web.Request.to_urlencoded req
        ||> HttpUtils.format_request_boolean_values Field.[ show FallbackToEmail ]
      in
      let* session = Session.find database_label session_id >|- Response.not_found in
      Response.bad_request_on_error ~urlencoded show
      @@
      let* message_channel = message_channel urlencoded |> Lwt_result.lift in
      let* assignments = assignments_from_requeset req database_label session_id in
      let tenant = Pool_context.Tenant.get_tenant_exn req in
      let open Message_template.ManualSessionMessage in
      let%lwt make_email_job = prepare tenant session in
      let open Pool_common.MessageChannel in
      let open Cqrs_command.Session_command in
      let* events =
        let open CCResult.Infix in
        let assignments = to_assignment_list assignments in
        match message_channel with
        | Email ->
          let open SendDirectEmailMessage in
          urlencoded
          |> decode
          >>= handle ~tags make_email_job assignments
          |> Lwt_result.lift
        | TextMessage ->
          let open SendDirectTextMessage in
          let%lwt make_sms_job = prepare_text_message tenant session in
          urlencoded
          |> decode
          >>= handle ~tags make_email_job make_sms_job assignments
          |> Lwt_result.lift
      in
      let%lwt () = Pool_event.handle_events ~tags database_label user events in
      HttpUtils.redirect_to_with_actions
        session_path
        [ Message.set ~success:[ Success.Sent Field.Message ] ]
      |> Lwt_result.ok
    in
    Response.handle ~src req result
  ;;
end

let update_matches_filter req =
  let open Utils.Lwt_result.Infix in
  let experiment_id = experiment_id req in
  let session_id = session_id req in
  let result { Pool_context.database_label; user; _ } =
    let tags = Pool_context.Logger.Tags.req req in
    let* session = Session.find database_label session_id in
    let* admin = Pool_context.get_admin_user user |> Lwt_result.lift in
    let* events =
      Assignment_job.update_matches_filter
        ~current_user:admin
        database_label
        (`Session session)
      >== Cqrs_command.Assignment_command.UpdateMatchesFilter.handle ~tags
    in
    let%lwt () = Pool_event.handle_events ~tags database_label user events in
    Response.Htmx.redirect
      ~actions:[ Message.set ~success:[ Success.Updated Field.Assignments ] ]
      (session_path ~id:session_id experiment_id)
    |> Lwt_result.ok
  in
  result |> Response.Htmx.handle ~src req
;;

module Tags = struct
  let handle action req =
    let experiment_id = experiment_id req in
    let session_id = session_id req in
    let redirect = session_path ~id:session_id experiment_id in
    Admin_experiments_tags.handle_tag action redirect edit req
  ;;

  let assign_session_participation_tag = handle `AssignSessionParticipationTag
  let remove_session_participation_tag = handle `RemoveSessionParticipationTag
end

module Api = struct
  let handle_request query_sessions req =
    let query_params = Sihl.Web.Request.query_list req in
    let find_param field =
      let open CCResult.Infix in
      HttpUtils.find_in_urlencoded field query_params
      >>= Pool_model.Time.parse_date_from_calendar
      |> Lwt_result.lift
    in
    let result { Pool_context.database_label; user; guardian; _ } =
      let* start_time = find_param Field.Start in
      let* end_time = find_param Field.End in
      let* actor =
        Pool_context.Utils.find_authorizable ~admin_only:true database_label user
      in
      query_sessions ~start_time ~end_time database_label actor guardian
      ||> CCList.map Session.Calendar.yojson_of_t
      ||> (fun json -> `List json)
      |> Lwt_result.ok
    in
    Response.Api.handle_in_tenant_context ~src req result
  ;;

  let current_user req =
    let query = Session.calendar_by_user in
    handle_request query req
  ;;

  let location req =
    let location_uuid = HttpUtils.find_id Pool_location.Id.of_string Field.Location req in
    let query = Session.calendar_by_location ~location_uuid in
    handle_request query req
  ;;

  module Access : sig
    val location : Rock.Middleware.t
  end = struct
    let location = Admin_location.Access.read
  end
end

module Access : sig
  include module type of Helpers.Access

  val reschedule : Rock.Middleware.t
  val cancel : Rock.Middleware.t
  val close : Rock.Middleware.t
  val direct_message : Rock.Middleware.t
  val update_matches_filter : Rock.Middleware.t
end = struct
  module SessionCommand = Cqrs_command.Session_command
  module AssignmentCommand = Cqrs_command.Assignment_command
  module Guardian = Middleware.Guardian
  open CCResult.Infix

  let find_id = HttpUtils.find_id
  let experiment_effects = Guardian.id_effects Experiment.Id.validate Field.Experiment

  let combined_effects validation_set =
    Guardian.validate_generic
    @@ fun req ->
    let* experiment_id = find_id Experiment.Id.validate Field.Experiment req in
    let* session_id = find_id Session.Id.validate Field.Session req in
    validation_set experiment_id session_id |> CCResult.return
  ;;

  let index = experiment_effects Session.Guard.Access.index
  let create = experiment_effects SessionCommand.Create.effects
  let read = combined_effects Session.Guard.Access.read
  let update = combined_effects SessionCommand.Update.effects
  let delete = combined_effects SessionCommand.Delete.effects
  let reschedule = combined_effects SessionCommand.Reschedule.effects
  let cancel = combined_effects SessionCommand.Cancel.effects
  let close = combined_effects SessionCommand.Close.effects

  let direct_message =
    let experiment_effects fcn req =
      let experiment_id = experiment_id req |> Experiment.Id.to_common in
      fcn experiment_id |> CCResult.return
    in
    Contact.Guard.Access.send_direct_message
    |> experiment_effects
    |> Guardian.validate_generic
  ;;

  let update_matches_filter =
    combined_effects AssignmentCommand.UpdateMatchesFilter.effects
  ;;
end
