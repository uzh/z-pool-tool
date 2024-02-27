open CCFun
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Field = Pool_common.Message.Field

let src = Logs.Src.create "handler.admin.session"
let create_layout req = General.create_tenant_layout req
let experiment_id = HttpUtils.find_id Experiment.Id.of_string Field.Experiment
let session_id = HttpUtils.find_id Session.Id.of_string Field.Session

let template_id =
  HttpUtils.find_id
    Message_template.Id.of_string
    Pool_common.Message.Field.MessageTemplate
;;

let session_path experiment_id session_id =
  Format.asprintf
    "/admin/experiments/%s/sessions/%s"
    (experiment_id |> Experiment.Id.value)
    (session_id |> Session.Id.value)
;;

let location urlencoded database_label =
  let open Utils.Lwt_result.Infix in
  let open Pool_common.Message in
  Field.(Location |> show)
  |> CCList.pure
  |> HttpUtils.urlencoded_to_params urlencoded
  |> CCOption.to_result (NotFound Field.Location)
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
  let chronological =
    Sihl.Web.Request.query Pool_common.Message.Field.(show Chronological) req
    |> CCOption.map_or ~default:false (CCString.equal "true")
  in
  let* experiment = Experiment.find database_label experiment_id in
  let flatten_sessions =
    CCList.fold_left
      (fun acc (parent, follow_ups) -> acc @ (parent :: follow_ups))
      []
  in
  let%lwt sessions =
    match chronological with
    | true -> Session.query_by_experiment ~query database_label experiment_id
    | false ->
      Session.query_grouped_by_experiment ~query database_label experiment_id
      ||> fun (sessions, query) -> flatten_sessions sessions, query
  in
  let open Page.Admin.Session in
  Lwt_result.ok
  @@
  match HttpUtils.Htmx.is_hx_request req with
  | true -> data_table context experiment sessions chronological |> Lwt.return
  | false -> index context experiment sessions chronological
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
       let%lwt locations = Pool_location.find_all database_label in
       let flash_fetcher = flip Sihl.Web.Flash.find req in
       let%lwt default_leadtime_settings =
         default_lead_time_settings database_label
       in
       let text_messages_enabled =
         Pool_context.Tenant.text_messages_enabled req
       in
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
           Page.Admin.Session.new_form
             context
             experiment
             default_leadtime_settings
             locations
             text_messages_enabled
             flash_fetcher
           |> Lwt_result.ok
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
    Format.asprintf
      "/admin/experiments/%s/sessions"
      (experiment_id |> Experiment.Id.value)
  in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, error_path)
    @@ let* experiment, session, followups, parent_session =
         duplication_session_data req database_label
       in
       Page.Admin.Session.duplicate
         context
         experiment
         session
         ?parent_session
         followups
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
      |> CCOption.to_result Pool_common.(Message.InvalidHtmxRequest)
      |> Lwt_result.lift
      >|+ ( + ) 1
    in
    Page.Admin.Session.duplicate_form
      ?parent_session
      language
      session
      followups
      counter
    |> HttpUtils.Htmx.html_to_plain_text_response
    |> Lwt_result.return
  in
  result |> HttpUtils.Htmx.handle_error_message ~src req
;;

let duplicate_post_htmx req =
  let open Utils.Lwt_result.Infix in
  let experiment_id = experiment_id req in
  let sessions_path =
    Format.asprintf
      "/admin/experiments/%s/sessions"
      (Experiment.Id.value experiment_id)
  in
  let result { Pool_context.database_label; _ } =
    let tags = Pool_context.Logger.Tags.req req in
    let%lwt urlencoded =
      Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
    in
    let* _, session, followups, parent_session =
      duplication_session_data req database_label
    in
    let* events =
      let open Cqrs_command.Session_command.Duplicate in
      urlencoded
      |> handle ~tags ?parent_session session followups
      |> Lwt_result.lift
    in
    let%lwt () = Pool_event.handle_events ~tags database_label events in
    HttpUtils.Htmx.htmx_redirect
      ~actions:
        [ Message.set ~success:[ Pool_common.Message.(Created Field.Sessions) ]
        ]
      sessions_path
      ()
    |> Lwt_result.ok
  in
  result
  |> HttpUtils.Htmx.handle_error_message ~error_as_notification:true ~src req
;;

let create req =
  let id = experiment_id req in
  let path =
    Format.asprintf "/admin/experiments/%s/sessions" (Experiment.Id.value id)
  in
  let result context =
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
    let database_label = context.Pool_context.database_label in
    let* location = location urlencoded database_label in
    let* experiment = Experiment.find database_label id in
    let* events =
      let open CCResult.Infix in
      let open Cqrs_command.Session_command.Create in
      urlencoded
      |> decode
      >>= handle ~tags experiment location
      |> Lwt_result.lift
    in
    let%lwt () = Pool_event.handle_events ~tags database_label events in
    Http_utils.redirect_to_with_actions
      path
      [ Message.set ~success:[ Pool_common.Message.(Created Field.Session) ] ]
    |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path_with_actions ~src req
;;

let session_page database_label req context session experiment =
  let open Utils.Lwt_result.Infix in
  let open Helpers.Guard in
  let session_id = Session.(session.id) in
  let experiment_id = Experiment.(experiment.id) in
  let experiment_target_id =
    [ Guard.Uuid.target_of Experiment.Id.value experiment_id ]
  in
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
    let%lwt locations = Pool_location.find_all database_label in
    let%lwt default_leadtime_settings =
      default_lead_time_settings database_label
    in
    let%lwt available_tags =
      Tags.ParticipationTags.(
        find_available
          database_label
          (Session (Session.Id.to_common session_id)))
    in
    let%lwt experiment_participation_tags =
      Tags.ParticipationTags.(
        find_all
          database_label
          (Experiment (Experiment.Id.to_common experiment_id)))
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
        find_all
          database_label
          (Experiment (Experiment.Id.to_common experiment_id)))
    in
    let%lwt counters =
      Assignment.counters_of_session database_label session_id
    in
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
    Page.Admin.Session.reschedule_session
      context
      experiment
      session
      flash_fetcher
    >|> create_layout
  | `Cancel ->
    let%lwt follow_ups = Session.find_follow_ups database_label session_id in
    Page.Admin.Session.cancel
      context
      experiment
      session
      follow_ups
      flash_fetcher
    >|> create_layout
  | `Print ->
    let%lwt assignments =
      Assignment.find_by_session database_label session.Session.id
    in
    Page.Admin.Session.print
      ~view_contact_name
      ~view_contact_info
      context
      experiment
      session
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
  let experiment_target_id =
    [ Guard.Uuid.target_of Experiment.Id.value experiment_id ]
  in
  HttpUtils.Htmx.handler
    ~error_path
    ~create_layout
    ~query:(module Assignment)
    req
  @@ fun ({ Pool_context.database_label; _ } as context) query ->
  let open Utils.Lwt_result.Infix in
  let* experiment = Experiment.find database_label experiment_id in
  let* session = Session.find database_label session_id in
  let view_contact_name = can_read_contact_name context experiment_target_id in
  let view_contact_info = can_read_contact_info context experiment_target_id in
  let access_contact_profiles =
    can_access_contact_profile context experiment_id
  in
  let text_messages_enabled = Pool_context.Tenant.text_messages_enabled req in
  let%lwt assignments =
    Assignment.query_by_session ~query database_label session_id
  in
  match HttpUtils.Htmx.is_hx_request req with
  | false ->
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
    Page.Admin.Session.detail
      ~access_contact_profiles
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
  | true ->
    Page.Admin.Assignment.(
      data_table
        ~access_contact_profiles
        ~view_contact_name
        ~view_contact_info
        Assignments
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
    (try experiment_id req |> CCOption.some with
     | _ -> None)
    |> CCOption.map_or
         ~default:"/admin/dashboard"
         (Experiment.Id.value
          %> Format.asprintf "/admin/experiments/%s/sessions")
  in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, error_path)
    @@
    let* session = Session.find database_label session_id in
    let* experiment =
      Experiment.find_of_session
        database_label
        (session_id |> Session.Id.to_common)
    in
    session_page database_label req context session experiment page
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
    let open Pool_common.Message in
    match action with
    | `Update -> "edit", Updated Field.session
    | `Reschedule -> "reschedule", Rescheduled Field.session
  in
  let result { Pool_context.database_label; _ } =
    let open Utils.Lwt_result.Infix in
    let%lwt urlencoded =
      Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
    in
    Utils.Lwt_result.map_error (fun err ->
      ( err
      , Format.asprintf "%s/%s" path error_path
      , [ HttpUtils.urlencoded_to_flash urlencoded ] ))
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let tenant = Pool_context.Tenant.get_tenant_exn req in
    let* session = Session.find database_label session_id in
    let* experiment = Experiment.find database_label experiment_id in
    let%lwt follow_ups =
      Session.find_follow_ups database_label session.Session.id
    in
    let* parent =
      match session.Session.follow_up_to with
      | None -> Lwt_result.return None
      | Some parent_id ->
        parent_id |> Session.find database_label >|+ CCOption.some
    in
    let* events =
      match action with
      | `Update ->
        let* location = location urlencoded database_label in
        let open CCResult.Infix in
        let open Cqrs_command.Session_command.Update in
        urlencoded
        |> decode
        >>= handle ~tags ?parent_session:parent follow_ups session location
        |> Lwt_result.lift
      | `Reschedule ->
        let open Cqrs_command.Session_command.Reschedule in
        let%lwt assignments =
          Assignment.find_uncanceled_by_session
            database_label
            session.Session.id
        in
        let system_languages =
          Pool_context.Tenant.get_tenant_languages_exn req
        in
        let%lwt admin_contact =
          Experiment.find_contact_person database_label experiment
        in
        let%lwt create_message =
          Message_template.SessionReschedule.prepare
            database_label
            tenant
            experiment
            system_languages
            session
            admin_contact
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
    let%lwt () = Pool_event.handle_events ~tags database_label events in
    Http_utils.redirect_to_with_actions
      path
      [ Message.set ~success:[ success_msg ] ]
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
    ||> HttpUtils.format_request_boolean_values
          Pool_common.Message.Field.[ show Email; show SMS ]
  in
  let result { Pool_context.database_label; _ } =
    Utils.Lwt_result.map_error (fun err ->
      err, error_path, [ HttpUtils.urlencoded_to_flash urlencoded ])
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let* experiment = Experiment.find database_label experiment_id in
    let* session = Session.find database_label session_id in
    let%lwt follow_ups =
      Session.find_follow_ups database_label session.Session.id
    in
    let%lwt assignments =
      session :: follow_ups
      |> Lwt_list.fold_left_s
           (fun assignments session ->
             Assignment.find_uncanceled_by_session
               database_label
               session.Session.id
             ||> CCList.append assignments)
           []
      ||> Assignment.group_by_contact
    in
    let* notify_via =
      let open Pool_common in
      Sihl.Web.Request.urlencoded_list
        Message.Field.(NotifyVia |> array_key)
        req
      ||> CCList.map NotifyVia.create
      ||> CCList.all_ok
      >|+ CCList.uniq ~eq:NotifyVia.equal
      >>= function
      | [] ->
        Lwt_result.fail Pool_common.Message.(NoOptionSelected Field.NotifyVia)
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
    let%lwt () = Pool_event.handle_events ~tags database_label events in
    Http_utils.redirect_to_with_actions
      success_path
      [ Message.set ~success:[ Pool_common.Message.(Canceled Field.Session) ] ]
    |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path_with_actions ~src req
;;

let delete req =
  let experiment_id = experiment_id req in
  let error_path =
    Format.asprintf
      "/admin/experiments/%s/sessions"
      (Experiment.Id.value experiment_id)
  in
  let result { Pool_context.database_label; _ } =
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
    let%lwt () = Pool_event.handle_events ~tags database_label events in
    Http_utils.redirect_to_with_actions
      error_path
      [ Message.set ~success:[ Pool_common.Message.(Deleted Field.Session) ] ]
    |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

(* TODO [aerben] make possible to create multiple follow ups? *)
let create_follow_up req =
  let experiment_id = experiment_id req in
  let session_id = session_id req in
  let path = session_path experiment_id session_id in
  let result { Pool_context.database_label; _ } =
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
    let%lwt () = Pool_event.handle_events ~tags database_label events in
    Http_utils.redirect_to_with_actions
      (Format.asprintf
         "/admin/experiments/%s/sessions"
         (Experiment.Id.value experiment_id))
      [ Message.set ~success:[ Pool_common.Message.(Created Field.Session) ] ]
    |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path_with_actions ~src req
;;

let close_post req =
  let experiment_id = experiment_id req in
  let session_id = session_id req in
  let path = session_path experiment_id session_id in
  let result { Pool_context.database_label; _ } =
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
           (fun tags entity ->
             find_all database_label entity ||> CCList.append tags)
           []
    in
    let* events =
      assignments
      |> Lwt_list.map_s
           (fun
               ({ Assignment.no_show; participated; contact; _ } as assignment)
             ->
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
                | true ->
                  find_follow_ups database_label assignment ||> CCOption.return
                | false -> Lwt.return_none
              in
              Lwt_result.return
                (assignment, increment_num_participations, follow_ups))
      ||> CCResult.flatten_l
      >== Close.handle experiment session participation_tags
    in
    let%lwt () = Pool_event.handle_events database_label events in
    Http_utils.redirect_to_with_actions
      path
      [ Message.set ~success:[ Pool_common.Message.(Closed Field.Session) ] ]
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
              [ Experiment.Id.to_common experiment_id
              ; Session.Id.to_common session_id
              ]
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

let delete_message_template req =
  let result { Pool_context.database_label; _ } =
    let experiment_id = experiment_id req in
    let session_id = session_id req in
    let template_id = template_id req in
    let redirect = session_path experiment_id session_id in
    Helpers.MessageTemplates.delete database_label template_id redirect
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
  let result { Pool_context.database_label; _ } =
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
      Reminder.prepare_messages
        database_label
        tenant
        tenant_languages
        experiment
        session
    in
    let* events =
      let open CCResult.Infix in
      let open Cqrs_command.Session_command.ResendReminders in
      urlencoded
      |> decode
      >>= handle ~tags create_messages session assignments
      |> Lwt_result.lift
    in
    let%lwt () = Pool_event.handle_events ~tags database_label events in
    Http_utils.redirect_to_with_actions
      path
      [ Message.set ~success:[ Pool_common.Message.RemindersResent ] ]
    |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path_with_actions ~src req
;;

module Api = struct
  let calendar_api ?actor req query =
    let result { Pool_context.database_label; guardian; _ } =
      let open Utils.Lwt_result.Infix in
      let query_params = Sihl.Web.Request.query_list req in
      let find_param field =
        let open CCResult.Infix in
        HttpUtils.find_in_urlencoded field query_params
        >>= Pool_common.Utils.Time.parse_date_from_calendar
        |> Lwt_result.lift
      in
      let* start_time = find_param Field.Start in
      let* end_time = find_param Field.End in
      let%lwt sessions =
        query database_label ~start_time ~end_time
        ||> CCList.map
              (fun
                  (Session.Calendar.{ id; experiment_id; location; links; _ } as
                   cal)
                ->
                 let open Session.Calendar in
                 match actor with
                 | None -> cal
                 | Some actor ->
                   let open Guard in
                   let session = Uuid.target_of Session.Id.value id in
                   let experiment =
                     Uuid.target_of Experiment.Id.value experiment_id
                   in
                   let location =
                     Uuid.target_of Pool_location.Id.value location.id
                   in
                   let check_guardian model target =
                     let open ValidationSet in
                     Persistence.PermissionOnTarget.validate_set
                       guardian
                       Pool_common.Message.authorization
                       (one_of_tuple (Permission.Read, model, Some target))
                       actor
                     |> CCResult.is_ok
                   in
                   let show_experiment =
                     check_guardian `Experiment experiment
                   in
                   let show_session = check_guardian `Session session in
                   let show_location_session =
                     check_guardian `Location location
                   in
                   let links =
                     { links with
                       show_session
                     ; show_experiment
                     ; show_location_session
                     }
                   in
                   { cal with links })
        ||> CCList.map Session.Calendar.yojson_of_t
      in
      `List sessions |> Lwt.return_ok
    in
    result |> HttpUtils.Json.handle_yojson_response ~src req
  ;;

  let location req =
    let location_id =
      HttpUtils.find_id Pool_location.Id.of_string Field.Location req
    in
    let query = Session.find_for_calendar_by_location location_id in
    calendar_api req query
  ;;

  let current_user req =
    let { Pool_context.database_label; user; language; _ } =
      Pool_context.find_exn req
    in
    let%lwt actor =
      Pool_context.Utils.find_authorizable ~admin_only:true database_label user
    in
    match actor with
    | Ok actor ->
      calendar_api ~actor req (Session.find_for_calendar_by_user actor)
    | Error err ->
      `Assoc
        [ "message", `String Pool_common.(Utils.error_to_string language err) ]
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
end = struct
  module SessionCommand = Cqrs_command.Session_command
  module Guardian = Middleware.Guardian

  let experiment_effects =
    Guardian.id_effects Experiment.Id.of_string Field.Experiment
  ;;

  let combined_effects fcn req =
    let open HttpUtils in
    let experiment_id = find_id Experiment.Id.of_string Field.Experiment req in
    let session_id = find_id Session.Id.of_string Field.Session req in
    fcn experiment_id session_id
  ;;

  let combined_with_location_effects fcn req =
    let open HttpUtils in
    let location_id = find_id Pool_location.Id.of_string Field.Location req in
    let session_id = find_id Session.Id.of_string Field.Session req in
    fcn location_id session_id
  ;;

  let index =
    let read id = Session.Guard.Access.index id in
    read |> experiment_effects |> Guardian.validate_generic ~any_id:true
  ;;

  let create =
    SessionCommand.Create.effects
    |> experiment_effects
    |> Guardian.validate_generic
  ;;

  let read =
    let read id = Session.Guard.Access.read id in
    read |> combined_effects |> Guardian.validate_generic
  ;;

  let read_by_location =
    let read id = Session.Guard.Access.read_by_location id in
    read |> combined_with_location_effects |> Guardian.validate_generic
  ;;

  let update =
    SessionCommand.Update.effects
    |> combined_effects
    |> Guardian.validate_generic
  ;;

  let delete =
    SessionCommand.Delete.effects
    |> combined_effects
    |> Guardian.validate_generic
  ;;

  let reschedule =
    SessionCommand.Reschedule.effects
    |> combined_effects
    |> Guardian.validate_generic
  ;;

  let cancel =
    SessionCommand.Cancel.effects
    |> combined_effects
    |> Guardian.validate_generic
  ;;

  let close =
    SessionCommand.Close.effects
    |> combined_effects
    |> Guardian.validate_generic
  ;;
end
