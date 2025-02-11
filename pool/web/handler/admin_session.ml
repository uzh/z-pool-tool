open CCFun
open Pool_message
module HttpUtils = Http_utils
module Message = HttpUtils.Message

let src = Logs.Src.create "handler.admin.session"
let create_layout req = General.create_tenant_layout req
let experiment_id = HttpUtils.find_id Experiment.Id.of_string Field.Experiment
let session_id = HttpUtils.find_id Session.Id.of_string Field.Session
let template_id = HttpUtils.find_id Message_template.Id.of_string Field.MessageTemplate

let session_path experiment_id session_id =
  Format.asprintf
    "/admin/experiments/%s/sessions/%s"
    (experiment_id |> Experiment.Id.value)
    (session_id |> Session.Id.value)
;;

let location urlencoded database_label =
  let open Utils.Lwt_result.Infix in
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

let list req =
  let experiment_id = experiment_id req in
  let error_path =
    Format.asprintf "/admin/experiments/%s" (Experiment.Id.value experiment_id)
  in
  HttpUtils.Htmx.handler ~error_path ~create_layout ~query:(module Session) req
  @@ fun ({ Pool_context.database_label; _ } as context) query ->
  let open Utils.Lwt_result.Infix in
  let* experiment = Experiment.find database_label experiment_id in
  let flatten_sessions =
    CCList.fold_left (fun acc (parent, follow_ups) -> acc @ (parent :: follow_ups)) []
  in
  Lwt_result.ok
  @@
  match Experiment.is_sessionless experiment with
  | true ->
    let to_html time_windows =
      let open Page.Admin.TimeWindow in
      match HttpUtils.Htmx.is_hx_request req with
      | true -> data_table context experiment time_windows |> Lwt.return
      | false -> index context experiment time_windows
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
      | true -> data_table context experiment sessions chronological |> Lwt.return
      | false -> index context experiment sessions chronological
    in
    (match chronological with
     | true -> Session.query_by_experiment ~query database_label experiment_id >|> to_html
     | false ->
       Session.query_grouped_by_experiment ~query database_label experiment_id
       ||> (fun (sessions, query) -> flatten_sessions sessions, query)
       >|> to_html)
;;

let new_helper req page =
  let open Utils.Lwt_result.Infix in
  let open Session in
  let id = experiment_id req in
  let error_path =
    Format.asprintf "/admin/experiments/%s/sessions" (id |> Experiment.Id.value)
  in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, error_path)
    @@ let* experiment = Experiment.find database_label id in
       let%lwt locations = Pool_location.all database_label in
       let flash_fetcher = flip Sihl.Web.Flash.find req in
       let%lwt default_leadtime_settings = default_lead_time_settings database_label in
       let text_messages_enabled = Pool_context.Tenant.text_messages_enabled req in
       let html =
         match page with
         | `FollowUp ->
           let session_id = session_id req in
           let* parent_session = find database_label session_id in
           Page.Admin.Session.follow_up
             context
             experiment
             default_leadtime_settings
             parent_session
             locations
             text_messages_enabled
             flash_fetcher
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
                 flash_fetcher
             | true -> Page.Admin.TimeWindow.new_form context experiment flash_fetcher)
       in
       html >>= create_layout req context >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let new_form req = new_helper req `New
let follow_up req = new_helper req `FollowUp

let duplication_session_data req database_label =
  let open Utils.Lwt_result.Infix in
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
  let open Utils.Lwt_result.Infix in
  let experiment_id = experiment_id req in
  let error_path =
    Format.asprintf "/admin/experiments/%s/sessions" (experiment_id |> Experiment.Id.value)
  in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, error_path)
    @@ let* experiment, session, followups, parent_session =
         duplication_session_data req database_label
       in
       Page.Admin.Session.duplicate context experiment session ?parent_session followups
       >|> create_layout req context
       >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let duplicate_form_htmx req =
  let open Utils.Lwt_result.Infix in
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
    |> HttpUtils.Htmx.html_to_plain_text_response
    |> Lwt_result.return
  in
  result |> HttpUtils.Htmx.handle_error_message ~src req
;;

let duplicate_post_htmx req =
  let open Utils.Lwt_result.Infix in
  let experiment_id = experiment_id req in
  let sessions_path =
    Format.asprintf "/admin/experiments/%s/sessions" (Experiment.Id.value experiment_id)
  in
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
    HttpUtils.Htmx.htmx_redirect
      ~actions:[ Message.set ~success:[ Success.Created Field.Sessions ] ]
      sessions_path
      ()
    |> Lwt_result.ok
  in
  result |> HttpUtils.Htmx.handle_error_message ~error_as_notification:true ~src req
;;

let create req =
  let id = experiment_id req in
  let path = Format.asprintf "/admin/experiments/%s/sessions" (Experiment.Id.value id) in
  let result { Pool_context.database_label; user; _ } =
    let open Utils.Lwt_result.Infix in
    let tags = Pool_context.Logger.Tags.req req in
    let%lwt urlencoded =
      Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
    in
    Utils.Lwt_result.map_error (fun err ->
      ( err
      , Format.asprintf "%s/%s" path "create"
      , [ HttpUtils.urlencoded_to_flash urlencoded ] ))
    @@
    let* experiment = Experiment.find database_label id in
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
      path
      [ Message.set ~success:[ Success.Created Field.Session ] ]
    |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path_with_actions ~src req
;;

let session_page database_label req context session experiment =
  let open Utils.Lwt_result.Infix in
  let open Helpers.Guard in
  let session_id = Session.(session.id) in
  let experiment_id = Experiment.(experiment.id) in
  let experiment_target_id = [ Guard.Uuid.target_of Experiment.Id.value experiment_id ] in
  let view_contact_name = can_read_contact_name context experiment_target_id in
  let view_contact_info = can_read_contact_info context experiment_target_id in
  let flash_fetcher = flip Sihl.Web.Flash.find req in
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
    let text_messages_enabled = Pool_context.Tenant.text_messages_enabled req in
    Page.Admin.Session.edit
      context
      experiment
      default_leadtime_settings
      session
      locations
      (current_tags, available_tags, experiment_participation_tags)
      text_messages_enabled
      flash_fetcher
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
    Page.Admin.Session.reschedule_session context experiment session flash_fetcher
    >|> create_layout
  | `Cancel ->
    let%lwt follow_ups = Session.find_follow_ups database_label session_id in
    Page.Admin.Session.cancel context experiment session follow_ups flash_fetcher
    >|> create_layout
  | `Print ->
    let%lwt assignments =
      Assignment.(
        find_for_session_detail_screen ~query:default_query database_label session_id)
    in
    Page.Admin.Session.print
      ~view_contact_name
      ~view_contact_info
      context
      experiment
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
  let flash_fetcher = flip Sihl.Web.Flash.find req in
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
    Page.Admin.TimeWindow.edit context experiment time_window tags flash_fetcher
    >|> create_layout
  | `Print ->
    let%lwt assignments =
      Assignment.(
        find_for_session_detail_screen ~query:default_query database_label time_window_id)
    in
    Page.Admin.Session.print
      ~view_contact_name
      ~view_contact_info
      context
      experiment
      (`TimeWindow time_window)
      assignments
    |> Lwt_result.return
;;

let show req =
  let open Helpers.Guard in
  let experiment_id = experiment_id req in
  let session_id = session_id req in
  let error_path =
    Format.asprintf "/admin/experiments/%s" (Experiment.Id.value experiment_id)
  in
  let experiment_target_id = [ Guard.Uuid.target_of Experiment.Id.value experiment_id ] in
  HttpUtils.Htmx.handler ~error_path ~create_layout ~query:(module Assignment) req
  @@ fun ({ Pool_context.database_label; _ } as context) query ->
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
  let text_messages_enabled = Pool_context.Tenant.text_messages_enabled req in
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
    let%lwt send_direct_message = Helpers.Guard.can_send_direct_message context in
    let%lwt rerun_session_filter =
      Helpers.Guard.can_rerun_session_filter context experiment_id session_id
    in
    (match session with
     | `Session session ->
       Page.Admin.Session.detail
         ~access_contact_profiles
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
        experiment
        session
        text_messages_enabled
        assignments)
    |> Lwt_result.return
;;

let detail page req =
  let open Utils.Lwt_result.Infix in
  let session_id = session_id req in
  let error_path =
    (try experiment_id req |> CCOption.return with
     | _ -> None)
    |> CCOption.map_or
         ~default:"/admin/dashboard"
         (Experiment.Id.value %> Format.asprintf "/admin/experiments/%s/sessions")
  in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, error_path)
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
  result |> HttpUtils.extract_happy_path ~src req
;;

let edit = detail `Edit
let reschedule_form = detail `Reschedule
let cancel_form = detail `Cancel
let close = detail `Close
let print = detail `Print

let update_handler action req =
  let experiment_id = experiment_id req in
  let session_id = session_id req in
  let path = session_path experiment_id session_id in
  let error_path, success_msg =
    match action with
    | `Update -> "edit", Success.Updated Field.session
    | `Reschedule -> "reschedule", Success.Rescheduled Field.session
  in
  let result { Pool_context.database_label; user; _ } =
    let open Utils.Lwt_result.Infix in
    let%lwt urlencoded =
      Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
    in
    Utils.Lwt_result.map_error (fun err ->
      ( err
      , Format.asprintf "%s/%s" path error_path
      , [ HttpUtils.urlencoded_to_flash urlencoded ] ))
    @@
    let sessions_data () =
      let* session = Session.find database_label session_id in
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
  result |> HttpUtils.extract_happy_path_with_actions ~src req
;;

let update = update_handler `Update
let reschedule = update_handler `Reschedule

let cancel req =
  let open Utils.Lwt_result.Infix in
  let experiment_id = experiment_id req in
  let session_id = session_id req in
  let success_path = session_path experiment_id session_id in
  let error_path = CCFormat.asprintf "%s/cancel" success_path in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req
    ||> HttpUtils.remove_empty_values
    ||> HttpUtils.format_request_boolean_values Field.[ show Email; show SMS ]
  in
  let result { Pool_context.database_label; user; _ } =
    Utils.Lwt_result.map_error (fun err ->
      err, error_path, [ HttpUtils.urlencoded_to_flash urlencoded ])
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let* experiment = Experiment.find database_label experiment_id in
    let* session = Session.find database_label session_id in
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
  result |> HttpUtils.extract_happy_path_with_actions ~src req
;;

let delete req =
  let experiment_id = experiment_id req in
  let error_path =
    Format.asprintf "/admin/experiments/%s/sessions" (Experiment.Id.value experiment_id)
  in
  let result { Pool_context.database_label; user; _ } =
    Utils.Lwt_result.map_error (fun err -> err, error_path)
    @@
    let open Utils.Lwt_result.Infix in
    let tags = Pool_context.Logger.Tags.req req in
    let session_id = session_id req in
    let* session = Session.find database_label session_id in
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
      error_path
      [ Message.set ~success:[ Success.Deleted Field.Session ] ]
    |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

(* TODO [aerben] make possible to create multiple follow ups? *)
let create_follow_up req =
  let experiment_id = experiment_id req in
  let session_id = session_id req in
  let path = session_path experiment_id session_id in
  let result { Pool_context.database_label; user; _ } =
    let open Utils.Lwt_result.Infix in
    let%lwt urlencoded =
      Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
    in
    Utils.Lwt_result.map_error (fun err ->
      ( err
      , Format.asprintf "%s/follow-up" path
      , [ HttpUtils.urlencoded_to_flash urlencoded ] ))
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let* location = location urlencoded database_label in
    let* session = Session.find database_label session_id in
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
      (Format.asprintf
         "/admin/experiments/%s/sessions"
         (Experiment.Id.value experiment_id))
      [ Message.set ~success:[ Success.Created Field.Session ] ]
    |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path_with_actions ~src req
;;

let close_post req =
  let experiment_id = experiment_id req in
  let session_id = session_id req in
  let path = session_path experiment_id session_id in
  let result { Pool_context.database_label; user; _ } =
    let open Utils.Lwt_result.Infix in
    Lwt_result.map_error (fun err -> err, Format.asprintf "%s/close" path)
    @@
    let open Cqrs_command.Session_command in
    let* experiment = Experiment.find database_label experiment_id in
    let* session = Session.find database_label session_id in
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
  result |> HttpUtils.extract_happy_path ~src req
;;

let message_template_form ?template_id label req =
  let open Utils.Lwt_result.Infix in
  let experiment_id = experiment_id req in
  let session_id = session_id req in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err ->
      ( err
      , experiment_id
        |> Experiment.Id.value
        |> Format.asprintf "/admin/experiments/%s/edit" ))
    @@
    let open Message_template in
    let flash_fetcher key = Sihl.Web.Flash.find key req in
    let tenant = Pool_context.Tenant.get_tenant_exn req in
    let* experiment = Experiment.find database_label experiment_id in
    let* session = Session.find database_label session_id in
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
      context
      tenant
      experiment
      session
      available_languages
      label
      form_context
      flash_fetcher
    >|> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let new_session_reminder req =
  message_template_form Message_template.Label.SessionReminder req
;;

let new_session_reminder_post req =
  let open Admin_message_templates in
  let experiment_id = experiment_id req in
  let session_id = session_id req |> Session.Id.to_common in
  let label = Message_template.Label.SessionReminder in
  let redirect =
    let base =
      Format.asprintf
        "/admin/experiments/%s/sessions/%s/%s"
        (Experiment.Id.value experiment_id)
        (Pool_common.Id.value session_id)
    in
    { success = base "edit"
    ; error = base (Message_template.Label.prefixed_human_url label)
    }
  in
  (write (Create (session_id, label, redirect))) req
;;

let edit_template req =
  let template_id = template_id req in
  message_template_form ~template_id Message_template.Label.SessionReminder req
;;

let update_template req =
  let open Admin_message_templates in
  let open Utils.Lwt_result.Infix in
  let open Message_template in
  let experiment_id = experiment_id req in
  let session_id = session_id req in
  let template_id = template_id req in
  let session_path =
    Format.asprintf
      "/admin/experiments/%s/sessions/%s/%s"
      (Experiment.Id.value experiment_id)
      (Session.Id.value session_id)
  in
  let%lwt template =
    req |> database_label_of_req |> Lwt_result.lift >>= flip find template_id
  in
  match template with
  | Ok template ->
    let redirect =
      { success = session_path "edit"
      ; error = session_path (prefixed_template_url ~append:"edit" template)
      }
    in
    (write (Update (template_id, redirect))) req
  | Error err ->
    HttpUtils.redirect_to_with_actions
      (session_path "edit")
      [ HttpUtils.Message.set ~error:[ err ] ]
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

let delete_message_template req =
  let result { Pool_context.database_label; user; _ } =
    let experiment_id = experiment_id req in
    let session_id = session_id req in
    let template_id = template_id req in
    let redirect = session_path experiment_id session_id in
    Helpers.MessageTemplates.delete database_label user template_id redirect
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let resend_reminders req =
  let experiment_id = experiment_id req in
  let session_id = session_id req in
  let path =
    Format.asprintf
      "/admin/experiments/%s/sessions/%s"
      (Experiment.Id.value experiment_id)
      (Session.Id.value session_id)
  in
  let result { Pool_context.database_label; user; _ } =
    let open Utils.Lwt_result.Infix in
    let tags = Pool_context.Logger.Tags.req req in
    let%lwt urlencoded =
      Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
    in
    Utils.Lwt_result.map_error (fun err ->
      ( err
      , Format.asprintf "%s/%s" path "create"
      , [ HttpUtils.urlencoded_to_flash urlencoded ] ))
    @@
    let* experiment = Experiment.find database_label experiment_id in
    let* session = Session.find database_label session_id in
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
      path
      [ Message.set ~success:[ Success.RemindersResent ] ]
    |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path_with_actions ~src req
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
    let open Utils.Lwt_result.Infix in
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
      let open Utils.Lwt_result.Infix in
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
      |> HttpUtils.Htmx.html_to_plain_text_response
      |> Lwt_result.return
    in
    result |> HttpUtils.Htmx.handle_error_message ~error_as_notification:true ~src req
  ;;

  let send req =
    let tags = Pool_context.Logger.Tags.req req in
    let experiment_id = experiment_id req in
    let session_id = session_id req in
    let session_path = session_path experiment_id session_id in
    let result { Pool_context.database_label; user; _ } =
      Lwt_result.map_error (fun err -> err, session_path)
      @@
      let open Utils.Lwt_result.Infix in
      let%lwt urlencoded =
        Sihl.Web.Request.to_urlencoded req
        ||> HttpUtils.format_request_boolean_values Field.[ show FallbackToEmail ]
      in
      let* message_channel = message_channel urlencoded |> Lwt_result.lift in
      let* assignments = assignments_from_requeset req database_label session_id in
      let* session = Session.find database_label session_id in
      let tenant = Pool_context.Tenant.get_tenant_exn req in
      let open Message_template.ManualSessionMessage in
      let%lwt make_email_job = prepare tenant session in
      let make_sms_job = prepare_text_message tenant session in
      let* events =
        let open CCResult.Infix in
        let open Cqrs_command.Session_command.SendDirectMessage in
        let assignments = to_assignment_list assignments in
        urlencoded
        |> decode message_channel
        >>= handle ~tags make_email_job make_sms_job assignments
        |> Lwt_result.lift
      in
      let%lwt () = Pool_event.handle_events ~tags database_label user events in
      HttpUtils.redirect_to_with_actions
        session_path
        [ Message.set ~success:[ Success.Sent Field.Message ] ]
      |> Lwt_result.ok
    in
    result |> HttpUtils.extract_happy_path ~src req
  ;;
end

let update_matches_filter req =
  let open Utils.Lwt_result.Infix in
  let experiment_id = experiment_id req in
  let session_id = session_id req in
  let path =
    Format.asprintf
      "/admin/experiments/%s/sessions/%s"
      (Experiment.Id.value experiment_id)
      (Session.Id.value session_id)
  in
  let result { Pool_context.database_label; user; _ } =
    let tags = Pool_context.Logger.Tags.req req in
    Utils.Lwt_result.map_error (fun err -> err, path)
    @@
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
    Http_utils.Htmx.htmx_redirect
      ~actions:[ Message.set ~success:[ Success.Updated Field.Assignments ] ]
      path
      ()
    |> Lwt_result.ok
  in
  result |> HttpUtils.Htmx.extract_happy_path ~src req
;;

module Api = struct
  let calendar_api ?actor req query =
    let result { Pool_context.database_label; guardian; _ } =
      let open Utils.Lwt_result.Infix in
      let query_params = Sihl.Web.Request.query_list req in
      let find_param field =
        let open CCResult.Infix in
        HttpUtils.find_in_urlencoded field query_params
        >>= Pool_model.Time.parse_date_from_calendar
        |> Lwt_result.lift
      in
      let* start_time = find_param Field.Start in
      let* end_time = find_param Field.End in
      let%lwt sessions =
        query database_label ~start_time ~end_time
        ||> CCList.map
              (fun (Session.Calendar.{ id; experiment_id; location; links; _ } as cal) ->
                 let open Session.Calendar in
                 match actor with
                 | None -> cal
                 | Some actor ->
                   let open Guard in
                   let session = Uuid.target_of Session.Id.value id in
                   let experiment = Uuid.target_of Experiment.Id.value experiment_id in
                   let location = Uuid.target_of Pool_location.Id.value location.id in
                   let check_guardian model target =
                     let open ValidationSet in
                     Persistence.PermissionOnTarget.validate_set
                       guardian
                       Error.authorization
                       (one_of_tuple (Permission.Read, model, Some target))
                       actor
                     |> CCResult.is_ok
                   in
                   let show_experiment = check_guardian `Experiment experiment in
                   let show_session = check_guardian `Session session in
                   let show_location_session = check_guardian `Location location in
                   let links =
                     { links with show_session; show_experiment; show_location_session }
                   in
                   { cal with links })
        ||> CCList.map Session.Calendar.yojson_of_t
      in
      `List sessions |> Lwt.return_ok
    in
    result |> HttpUtils.Json.handle_yojson_response ~src req
  ;;

  let location req =
    let location_id = HttpUtils.find_id Pool_location.Id.of_string Field.Location req in
    let query = Session.find_for_calendar_by_location location_id in
    calendar_api req query
  ;;

  let current_user req =
    let { Pool_context.database_label; user; language; _ } = Pool_context.find_exn req in
    let%lwt actor =
      Pool_context.Utils.find_authorizable ~admin_only:true database_label user
    in
    match actor with
    | Ok actor -> calendar_api ~actor req (Session.find_for_calendar_by_user actor)
    | Error err ->
      `Assoc [ "message", `String Pool_common.(Utils.error_to_string language err) ]
      |> HttpUtils.Json.yojson_response ~status:(Opium.Status.of_code 400)
  ;;

  module Access : sig
    val location : Rock.Middleware.t
  end = struct
    let location = Admin_location.Access.read
  end
end

module Access : sig
  include module type of Helpers.Access

  val read_by_location : Rock.Middleware.t
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

  let combined_with_location_effects validation_set =
    Guardian.validate_generic
    @@ fun req ->
    let* location_id = find_id Pool_location.Id.validate Field.Location req in
    let* session_id = find_id Session.Id.validate Field.Session req in
    validation_set location_id session_id |> CCResult.return
  ;;

  let index = experiment_effects Session.Guard.Access.index
  let create = experiment_effects SessionCommand.Create.effects
  let read = combined_effects Session.Guard.Access.read

  let read_by_location =
    combined_with_location_effects Session.Guard.Access.read_by_location
  ;;

  let update = combined_effects SessionCommand.Update.effects
  let delete = combined_effects SessionCommand.Delete.effects
  let reschedule = combined_effects SessionCommand.Reschedule.effects
  let cancel = combined_effects SessionCommand.Cancel.effects
  let close = combined_effects SessionCommand.Close.effects

  let direct_message =
    Contact.Guard.Access.send_direct_message |> Guardian.validate_admin_entity
  ;;

  let update_matches_filter =
    combined_effects AssignmentCommand.UpdateMatchesFilter.effects
  ;;
end
