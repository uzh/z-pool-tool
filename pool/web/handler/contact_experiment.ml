open Pool_message
module HttpUtils = Http_utils

let src = Logs.Src.create "handler.contact.assignment"
let create_layout = Contact_general.create_layout
let experiment_id = HttpUtils.find_id Experiment.Id.of_string Field.Experiment
let assignment_id = HttpUtils.find_id Assignment.Id.of_string Field.Assignment

let index req =
  let open Utils.Lwt_result.Infix in
  let error_path = "/" in
  let result ({ Pool_context.database_label; language; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, error_path)
    @@ let* contact = Pool_context.find_contact context |> Lwt_result.lift in
       let%lwt experiment_list =
         Experiment.find_upcoming_to_register database_label contact `OnSite
       in
       let%lwt online_studies =
         Experiment.find_upcoming_to_register database_label contact `Online
       in
       let* upcoming_sessions =
         Session.find_upcoming_public_by_contact
           database_label
           (Contact.id contact)
       in
       let%lwt past_experiments =
         Experiment.find_past_experiments_by_contact database_label contact
       in
       let%lwt custom_fields_anwsered =
         Custom_field.all_answered database_label (Contact.id contact)
       in
       let%lwt waiting_list =
         Experiment.find_pending_waitinglists_by_contact database_label contact
       in
       let%lwt i18n =
         let find key = I18n.find_by_key database_label key language in
         let%lwt upcoming_sessions = find I18n.Key.DashboardUpcomingSessions in
         let%lwt online_studies = find I18n.Key.DashboardOnlineStudies in
         let%lwt experiment_registration =
           find I18n.Key.DashboardExperimentRegistration
         in
         let%lwt experiment_history =
           find I18n.Key.DashboardExperimentHistory
         in
         let%lwt waiting_list = find I18n.Key.DashboardWaitinglist in
         Lwt.return
           Page.Contact.Experiment.
             { upcoming_sessions
             ; online_studies
             ; experiment_registration
             ; experiment_history
             ; waiting_list
             }
       in
       Page.Contact.Experiment.index
         experiment_list
         online_studies
         upcoming_sessions
         waiting_list
         past_experiments
         custom_fields_anwsered
         i18n
         context
       |> create_layout ~active_navigation:"/experiments" req context
       >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let show_online_study
  req
  ({ Pool_context.database_label; _ } as context)
  experiment
  contact
  =
  let open Utils.Lwt_result.Infix in
  let experiment_id = Experiment.Public.id experiment in
  let* time_window =
    Time_window.find_current_by_experiment database_label experiment_id
    >|- CCFun.const (Error.NotFound Field.Experiment)
  in
  let%lwt assignment =
    let open Utils.Lwt_result.Infix in
    Assignment.Public.find_all_by_experiment
      database_label
      experiment_id
      contact
    ||> CCList.head_opt
  in
  let argument =
    let open CCOption in
    let open Assignment in
    match assignment with
    | None -> `Upcoming time_window
    | Some assignment ->
      assignment
      |> Public.participated
      >|= Participated.value
      |> value ~default:false
      |> (function
       | true -> `Participated assignment
       | false -> `Pending (assignment, time_window))
  in
  Page.Contact.Experiment.show_online_study experiment context argument
  |> Lwt.return_ok
  >>= create_layout req context
  >|+ Sihl.Web.Response.of_html
;;

let show req =
  let open CCFun in
  let open Utils.Lwt_result.Infix in
  let error_path = "/experiments" in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, error_path)
    @@
    let id = experiment_id req in
    let* contact = Pool_context.find_contact context |> Lwt_result.lift in
    let* experiment = Experiment.find_public database_label id contact in
    match Experiment.Public.is_sessionless experiment with
    | true -> show_online_study req context experiment contact
    | false ->
      let* grouped_sessions =
        Session.find_all_public_for_experiment database_label contact id
        >|+ Session.Public.group_and_sort
        >|+ CCList.filter (fst %> Session.Public.is_fully_booked %> not)
      in
      let find_sessions fnc =
        let open Assignment in
        fnc database_label id contact
        >|> Lwt_list.map_s (fun { Public.id; _ } ->
          id |> Id.to_common |> Session.find_public_by_assignment database_label)
        ||> CCResult.flatten_l
      in
      let* upcoming_sessions =
        find_sessions Assignment.Public.find_upcoming_by_experiment
      in
      let* past_sessions =
        find_sessions Assignment.Public.find_past_by_experiment
      in
      let* canceled_sessions =
        find_sessions Assignment.Public.find_canceled_by_experiment
      in
      let%lwt user_is_on_waiting_list =
        Waiting_list.user_is_enlisted database_label contact id
      in
      Page.Contact.Experiment.show
        experiment
        grouped_sessions
        upcoming_sessions
        past_sessions
        canceled_sessions
        user_is_on_waiting_list
        contact
        context
      |> Lwt.return_ok
      >>= create_layout req context
      >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

module OnlineSurvey = struct
  module Command = Cqrs_command.Assignment_command.OnlineSurvey

  let redirect req =
    let open Utils.Lwt_result.Infix in
    let error_path = "/experiments" in
    let result ({ Pool_context.database_label; user; _ } as context) =
      Utils.Lwt_result.map_error (fun err -> err, error_path)
      @@
      let open Experiment in
      let tags = Pool_context.Logger.Tags.req req in
      let experiment_id = experiment_id req in
      let tenant = Pool_context.Tenant.get_tenant_exn req in
      let* contact = Pool_context.find_contact context |> Lwt_result.lift in
      let* experiment =
        Experiment.find_public database_label experiment_id contact
      in
      let%lwt assignment =
        let open Utils.Lwt_result.Infix in
        Assignment.Public.find_all_by_experiment
          database_label
          experiment_id
          contact
        ||> CCList.head_opt
      in
      let assignment_id =
        CCOption.map_or
          ~default:(Assignment.Id.create ())
          (fun { Assignment.Public.id; _ } -> id)
          assignment
      in
      let* survey_url =
        let open Public in
        experiment
        |> online_experiment
        |> CCOption.to_result (Error.NotFound Field.Experiment)
        |> Lwt_result.lift
        >|+ OnlineExperiment.survey_url
        >|+ OnlineExperiment.render_survey_url
              tenant
              ~experiment_id
              ~assignment_id:(Assignment.Id.to_common assignment_id)
      in
      let* time_window =
        Time_window.find_current_by_experiment
          database_label
          (Experiment.Public.id experiment)
        >|- CCFun.const (Error.NotFound Field.Experiment)
      in
      let* events =
        Lwt_result.lift
        @@
        match assignment with
        | Some _ -> Ok []
        | None ->
          let open Command.Create in
          handle ~id:assignment_id ~tags { contact; time_window; experiment }
      in
      let handle events =
        let%lwt () =
          Pool_event.handle_events ~tags database_label user events
        in
        Sihl.Web.Response.redirect_to survey_url |> Lwt_result.return
      in
      events |> handle
    in
    result |> HttpUtils.extract_happy_path ~src req
  ;;

  let submit req =
    let open Utils.Lwt_result.Infix in
    let error_path = "/" in
    let result ({ Pool_context.database_label; user; _ } as context) =
      Utils.Lwt_result.map_error (fun err -> err, error_path)
      @@
      let tags = Pool_context.Logger.Tags.req req in
      let assignment_id = assignment_id req in
      let experiment_id = experiment_id req in
      let* assignment = Assignment.find database_label assignment_id in
      let* experiment =
        Experiment.find_public
          database_label
          experiment_id
          assignment.Assignment.contact
      in
      let query = Sihl.Web.Request.query_list req in
      let* events =
        let open Command.Submit in
        let open CCResult.Infix in
        query |> decode >>= handle ~tags assignment |> Lwt_result.lift
      in
      let handle = Pool_event.handle_events ~tags database_label user in
      let return () =
        Page.Contact.Experiment.online_study_completition experiment context
        |> Lwt.return_ok
        >>= create_layout req context
        >|+ Sihl.Web.Response.of_html
      in
      events |> handle >|> return
    in
    result |> HttpUtils.extract_happy_path ~src req
  ;;
end
