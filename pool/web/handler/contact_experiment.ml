open Utils.Lwt_result.Infix
open Pool_message
module HttpUtils = Http_utils
module Response = Http_response

let src = Logs.Src.create "handler.contact.assignment"
let create_layout = Contact_general.create_layout
let experiment_id = HttpUtils.find_id Experiment.Id.of_string Field.Experiment
let assignment_id = HttpUtils.find_id Assignment.Id.of_string Field.Assignment
let experiment_path = HttpUtils.Url.Contact.experiment_path

let context_user context =
  Pool_context.find_contact context
  |> Lwt_result.lift
  >|- CCFun.const Response.access_denied
;;

let dashboard req =
  let result ({ Pool_context.database_label; language; _ } as context) =
    let* contact = context_user context in
    Response.bad_request_render_error context
    @@
    let%lwt upcoming_sessions =
      let open Session in
      let query =
        let open Query in
        let filter =
          let open Filter in
          Condition.[ Checkbox (Public.column_past, true) ]
        in
        let pagination = Pagination.create ~limit:2 ~page:0 () in
        create ~pagination ~filter ()
      in
      query_by_contact ~query database_label contact
    in
    let%lwt experiment_list =
      Experiment.find_upcoming database_label (`Dashboard 2) contact `OnSite
    in
    let%lwt online_studies =
      Experiment.find_upcoming database_label (`Dashboard 2) contact `Online
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
      let%lwt experiment_registration = find I18n.Key.DashboardExperimentRegistration in
      let%lwt experiment_history = find I18n.Key.DashboardExperimentHistory in
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
      custom_fields_anwsered
      i18n
      context
    |> create_layout ~active_navigation:"/experiments" req context
    >|+ Sihl.Web.Response.of_html
  in
  Response.handle ~src req result
;;

let index_handler page_context req =
  Response.Htmx.index_handler ~create_layout ~query:(module Experiment.Public) req
  @@ fun ({ Pool_context.database_label; user; language; _ } as context) query ->
  let open Utils.Lwt_result.Infix in
  let* contact = Pool_context.get_contact_user user |> Lwt_result.lift in
  let%lwt experiments =
    let context =
      match page_context with
      | `UpcomingOnsite -> `OnSite
      | `UpcomingOnline -> `Online
    in
    Experiment.find_upcoming database_label (`Query query) contact context
  in
  let page_title () =
    let label =
      match page_context with
      | `UpcomingOnsite -> I18n.Key.DashboardExperimentRegistration
      | `UpcomingOnline -> I18n.Key.DashboardOnlineStudies
    in
    I18n.find_by_key database_label label language
  in
  let open Page.Contact.Experiment in
  let page_context = (page_context :> experiment_list) in
  let%lwt page =
    match HttpUtils.Htmx.is_hx_request req with
    | true -> list |> Lwt.return
    | false ->
      let%lwt title = page_title () in
      experiment_list title |> Lwt.return
  in
  page page_context context experiments |> Lwt.return_ok
;;

let available_onsite = index_handler `UpcomingOnsite
let available_online = index_handler `UpcomingOnline

let history req =
  Response.Htmx.index_handler ~create_layout ~query:(module Experiment) req
  @@ fun ({ Pool_context.database_label; user; language; _ } as context) query ->
  let open Utils.Lwt_result.Infix in
  let* contact = Pool_context.get_contact_user user |> Lwt_result.lift in
  let%lwt experiments =
    Experiment.query_participation_history_by_contact ~query database_label contact
  in
  let open Page.Contact.Experiment.History in
  let%lwt page =
    match HttpUtils.Htmx.is_hx_request req with
    | true -> list |> Lwt.return
    | false ->
      let%lwt title =
        I18n.find_by_key database_label I18n.Key.DashboardExperimentHistory language
      in
      show title |> Lwt.return
  in
  page context experiments |> Lwt.return_ok
;;

let show_online_study
      ({ Pool_context.database_label; _ } as context)
      experiment
      matches_filter
      contact
  =
  let open Utils.Lwt_result.Infix in
  let experiment_id = Experiment.Public.id experiment in
  let%lwt assignment =
    Assignment.Public.find_all_by_experiment database_label experiment_id contact
    ||> CCList.head_opt
  in
  let%lwt current_time_window =
    Time_window.find_current_by_experiment database_label experiment_id
  in
  let%lwt upcoming_time_window =
    Time_window.find_upcoming_by_experiment database_label experiment_id
  in
  let argument =
    let open CCOption in
    let open Assignment in
    match assignment with
    | Some assignment ->
      assignment
      |> Public.participated
      >|= Participated.value
      |> value ~default:false
      |> (function
       | true -> `Participated assignment
       | false ->
         (match current_time_window with
          | Some time_window -> `Active (time_window, Some assignment)
          | None -> `Upcoming upcoming_time_window))
    | None ->
      (match current_time_window with
       | Some time_window -> `Active (time_window, None)
       | None -> `Upcoming upcoming_time_window)
  in
  Page.Contact.Experiment.Detail.online_study experiment matches_filter context argument
  |> Lwt.return_ok
;;

let show_onsite_study
      id
      ({ Pool_context.database_label; _ } as context)
      experiment
      matches_filter
      contact
  =
  let open Utils.Lwt_result.Infix in
  let* grouped_sessions =
    Session.find_all_public_for_experiment database_label contact id
    >|+ Session.Public.group_and_sort
    >|+ CCList.filter CCFun.(fst %> Session.Public.is_fully_booked %> not)
  in
  let find_sessions = Session.find_by_contact_and_experiment database_label contact id in
  let%lwt upcoming_sessions = find_sessions `Upcoming in
  let%lwt past_sessions = find_sessions `Past in
  let%lwt canceled_sessions = find_sessions `Canceled in
  let%lwt user_is_on_waiting_list =
    Waiting_list.user_is_enlisted database_label contact id
  in
  Page.Contact.Experiment.Detail.onsite_study
    experiment
    matches_filter
    grouped_sessions
    upcoming_sessions
    past_sessions
    canceled_sessions
    user_is_on_waiting_list
    contact
    context
  |> Lwt.return_ok
;;

let show req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.database_label; _ } as context) =
    let id = experiment_id req in
    let* contact = context_user context in
    let* experiment =
      Experiment.find_public database_label id contact >|- Response.not_found
    in
    Response.bad_request_render_error context
    @@
    let%lwt matches_filter =
      Experiment.Public.contact_matches_filter database_label experiment contact
    in
    (match Experiment.Public.is_sessionless experiment with
     | true -> show_online_study context
     | false -> show_onsite_study id context)
      experiment
      matches_filter
      contact
    >>= create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  Response.handle ~src req result
;;

module OnlineSurvey = struct
  module Command = Cqrs_command.Assignment_command.OnlineSurvey

  let redirect req =
    let open Utils.Lwt_result.Infix in
    let result ({ Pool_context.database_label; user; _ } as context) =
      let experiment_id = experiment_id req in
      let* contact =
        Pool_context.find_contact context
        |> Lwt_result.lift
        >|- CCFun.const Response.access_denied
      in
      let* experiment =
        Experiment.find_public database_label experiment_id contact >|- Response.not_found
      in
      Response.bad_request_render_error context
      @@
      let open Experiment in
      let tags = Pool_context.Logger.Tags.req req in
      let tenant = Pool_context.Tenant.get_tenant_exn req in
      let%lwt assignment =
        let open Utils.Lwt_result.Infix in
        Assignment.Public.find_all_by_experiment database_label experiment_id contact
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
        ||> CCOption.to_result Pool_message.(Error.NotFound Field.Experiment)
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
        let%lwt () = Pool_event.handle_events ~tags database_label user events in
        Sihl.Web.Response.redirect_to survey_url |> Lwt_result.return
      in
      events |> handle
    in
    Response.handle ~src req result
  ;;

  let submit req =
    let open Utils.Lwt_result.Infix in
    let result ({ Pool_context.database_label; user; _ } as context) =
      let assignment_id = assignment_id req in
      let experiment_id = experiment_id req in
      let* assignment =
        Assignment.find database_label assignment_id >|- Response.not_found
      in
      let* experiment =
        Experiment.find_public database_label experiment_id assignment.Assignment.contact
        >|- Response.not_found
      in
      Response.bad_request_render_error context
      @@
      let tags = Pool_context.Logger.Tags.req req in
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
    Response.handle ~src req result
  ;;
end
