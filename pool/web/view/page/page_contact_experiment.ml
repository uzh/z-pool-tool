open CCFun
open Tyxml.Html
open Component.Input
open Pool_message
module PageSession = Page_contact_sessions
module HttpUtils = Http_utils

type dashboard_i18n =
  { upcoming_sessions : I18n.t
  ; online_studies : I18n.t
  ; experiment_registration : I18n.t
  ; experiment_history : I18n.t
  ; waiting_list : I18n.t
  }

type experiment_list =
  [ `Participated
  | `UpcomingOnsite
  | `UpcomingOnline
  ]

let experiment_list_url context =
  let suffix =
    match context with
    | `UpcomingOnsite -> "available-onsite"
    | `UpcomingOnline -> "available-online"
    | `Participated -> "participated"
  in
  HttpUtils.Url.Contact.experiment_path ~suffix ()
;;

let experiment_public_description =
  let open Experiment in
  Public.description
  %> CCOption.map_or
       ~default:(txt "")
       (PublicDescription.value
        %> HttpUtils.add_line_breaks
        %> CCList.return
        %> div ~a:[ a_class [ "truncate-6" ] ])
;;

let experiment_title =
  let open Experiment in
  Public.public_title %> PublicTitle.value %> txt
;;

let not_matching_warning language =
  Component.Notification.notification
    language
    `Error
    [ txt
        Pool_common.(
          Utils.hint_to_string language I18n.ContactExperimentNotMatchingFilter)
    ]
;;

let experiment_detail_page experiment html =
  div
    ~a:[ a_class [ "trim"; "measure"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1"; "has-gap"; "word-wrap-break" ] ]
        [ experiment |> experiment_title ]
    ; experiment |> experiment_public_description
    ; html
    ]
;;

let make_panel ~query_parameters url content =
  a
    ~a:
      [ a_href (HttpUtils.externalize_path_with_params query_parameters url)
      ; a_class [ "panel"; "flexrow"; "flex-gap"; "inset-sm" ]
      ]
    [ div ~a:[ a_class [ "grow" ] ] content; Component.Icon.(to_html ChevronForward) ]
;;

let index
      experiment_list
      online_studies
      upcoming_sessions
      waiting_list
      past_experiments
      custom_fields_ansered
      i18n
      Pool_context.{ language; query_parameters; _ }
  =
  let total_link_from_query url to_html { Query.pagination; _ } =
    let open Query in
    let open CCOption in
    let total = pagination >|= fun { Pagination.item_count; _ } -> item_count in
    match total with
    | None -> []
    | Some total ->
      let html = [ to_html total ] in
      [ make_panel ~query_parameters url html ]
  in
  let list_html ?empty_msg ?note title classnames list =
    div
      [ h2
          ~a:[ a_class [ "heading-2"; "has-gap" ] ]
          [ txt (I18n.content_to_string title) ]
      ; div
          ~a:[ a_class [ "stack" ] ]
          [ CCOption.map_or
              ~default:(txt "")
              (fun hint ->
                 hint
                 |> Pool_common.Utils.hint_to_string language
                 |> txt
                 |> CCList.return
                 |> p)
              note
          ; (match list, empty_msg with
             | [], Some empty_msg ->
               p Pool_common.[ Utils.text_to_string language empty_msg |> txt ]
             | [], None -> txt ""
             | list, _ -> div ~a:[ a_class classnames ] list)
          ]
      ]
  in
  let notification =
    let custom_fields_ansered =
      if custom_fields_ansered
      then None
      else (
        let text =
          [ div
              [ txt
                  Pool_common.(
                    Utils.text_to_string language I18n.DashboardProfileCompletionText)
              ]
          ]
        in
        Some
          (Component.Notification.notification
             ~link:("/user/personal-details", Pool_common.I18n.PersonalDetails)
             language
             `Warning
             text))
    in
    [ custom_fields_ansered ]
    |> CCList.filter_map CCFun.id
    |> div ~a:[ a_class [ "stack" ] ]
  in
  let experiment_item experiment =
    let open Experiment in
    let html =
      [ div
          ~a:[ a_class [ "flexcolumn"; "stack-xs" ] ]
          [ h4
              ~a:[ a_class [ "word-wrap-break" ] ]
              [ txt (experiment |> Public.public_title |> PublicTitle.value) ]
          ; experiment_public_description experiment
          ]
      ]
    in
    make_panel
      ~query_parameters
      (HttpUtils.Url.Contact.experiment_path ~id:(Public.id experiment) ())
      html
  in
  let open Pool_common.I18n in
  let experiment_html =
    let experiments, query = experiment_list in
    let total =
      let make_link total =
        strong
          [ txt
              Pool_common.(
                Utils.control_to_string language Control.(AllAvailableExperiments total))
          ]
      in
      total_link_from_query (experiment_list_url `UpcomingOnsite) make_link query
    in
    let panel =
      let sessions = CCList.map experiment_item experiments in
      list_html
        i18n.experiment_registration
        ~note:ExperimentSessionsPublic
        ~empty_msg:ExperimentListEmpty
        [ "panel-list" ]
        (sessions @ total)
    in
    div [ panel ]
  in
  let online_studies_html =
    let experiments, query = online_studies in
    let total =
      let make_link total =
        strong
          [ txt
              Pool_common.(
                Utils.control_to_string language Control.(AllAvailableExperiments total))
          ]
      in
      total_link_from_query (experiment_list_url `UpcomingOnline) make_link query
    in
    let panel =
      let sessions = CCList.map experiment_item experiments in
      list_html
        i18n.online_studies
        ~empty_msg:ExperimentOnlineListEmpty
        [ "panel-list" ]
        (sessions @ total)
    in
    div [ panel ]
  in
  let past_experiments_html =
    match past_experiments with
    | [] -> txt ""
    | past_experiments ->
      past_experiments
      |> CCList.map experiment_item
      |> list_html i18n.experiment_history [ "striped" ]
  in
  let session_html =
    let upcoming_session { Session.Public.experiment_id; start; location; _ } =
      let open Session in
      let make_panel experiment_id =
        make_panel
          ~query_parameters
          (HttpUtils.Url.Contact.experiment_path ~id:experiment_id ())
      in
      let html =
        [ div
            ~a:[ a_class [ "flexcolumn"; "stack-xs" ] ]
            [ p [ txt (Start.value start |> Pool_model.Time.formatted_date_time) ]
            ; p [ txt (location.Pool_location.name |> Pool_location.Name.value) ]
            ]
        ]
      in
      make_panel experiment_id html
    in
    let sessions, query = upcoming_sessions in
    let total =
      let make_link total =
        strong
          [ txt Pool_common.(Utils.control_to_string language Control.(AllSessions total))
          ]
      in
      total_link_from_query (HttpUtils.Url.Contact.session_path ()) make_link query
    in
    let panel =
      let sessions = CCList.map upcoming_session sessions in
      list_html
        i18n.upcoming_sessions
        ~empty_msg:UpcomingSessionsListEmpty
        [ "panel-list" ]
        (sessions @ total)
    in
    div [ panel ]
  in
  let waiting_list_html =
    match waiting_list with
    | [] -> txt ""
    | list ->
      let open Pool_common.I18n in
      list
      |> CCList.map experiment_item
      |> list_html i18n.waiting_list ~empty_msg:ContactWaitingListEmpty [ "striped" ]
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1"; "has-gap" ] ]
        [ txt Pool_common.(Utils.text_to_string language I18n.DashboardTitle) ]
    ; div
        ~a:[ a_class [ "stack-lg" ] ]
        [ notification
        ; div
            ~a:[ a_class [ "grid-col-2"; "grid-gap-lg"; "grid-row-gap-xl"; "gap-lg" ] ]
            [ div ~a:[ a_class [ "stack-lg" ] ] [ session_html; waiting_list_html ]
            ; experiment_html
            ; online_studies_html
            ; past_experiments_html
            ]
        ]
    ]
;;

let show
      experiment
      matches_filter
      grouped_sessions
      upcoming_sessions
      past_sessions
      canceled_sessions
      user_is_enlisted
      contact
      Pool_context.{ language; query_parameters; csrf; _ }
  =
  let open Pool_common in
  let hint_to_string = Utils.hint_to_string language in
  let form_control, submit_class =
    match user_is_enlisted with
    | true -> Control.RemoveFromWaitingList, "error"
    | false -> Control.AddToWaitingList, "primary"
  in
  let session_list sessions =
    div
      ~a:[ a_class [ "stack" ] ]
      ([ h2
           ~a:[ a_class [ "heading-2" ] ]
           [ txt (Utils.nav_link_to_string language I18n.Sessions) ]
       ; p [ txt (hint_to_string I18n.ExperimentSessionsPublic) ]
       ]
       @
       if CCList.is_empty sessions
       then
         [ p
             ~a:[ a_class [ "gap" ] ]
             [ Utils.text_to_string language (I18n.EmtpyList Field.Sessions) |> txt ]
         ]
       else [ div [ PageSession.public_overview sessions experiment language ] ])
  in
  let waiting_list_form () =
    let form_action =
      HttpUtils.Url.Contact.experiment_path
        ~id:(Experiment.Public.id experiment)
        ~suffix:"waiting-list"
        ()
      |> (fun url -> if user_is_enlisted then Format.asprintf "%s/remove" url else url)
      |> HttpUtils.externalize_path_with_params query_parameters
    in
    let text_blocks =
      let base =
        (if user_is_enlisted then I18n.ContactOnWaitingList else I18n.SignUpForWaitingList)
        |> fun msg -> p [ txt (hint_to_string msg) ]
      in
      let missing_phone =
        if CCOption.is_none contact.Contact.cell_phone
        then
          [ Component.Notification.notification
              ~link:
                ( HttpUtils.url_with_field_params
                    query_parameters
                    "/user/contact-information"
                , I18n.PersonalDetails )
              language
              `Warning
              [ txt (hint_to_string I18n.WaitingListPhoneMissingContact) ]
          ]
        else []
      in
      div (missing_phone @ [ base ])
    in
    div
      ~a:[ a_class [ "stack" ] ]
      [ h2
          ~a:[ a_class [ "heading-2"; "has-gap" ] ]
          [ txt (Utils.text_to_string language I18n.ExperimentWaitingListTitle) ]
      ; text_blocks
      ; form
          ~a:[ a_method `Post; a_action form_action ]
          [ csrf_element csrf ()
          ; div
              ~a:[ a_class [ "flexrow" ] ]
              [ submit_element
                  language
                  form_control
                  ~classnames:[ submit_class; "push" ]
                  ()
              ]
          ]
      ]
  in
  let sessions_html title = function
    | [] -> txt ""
    | sessions ->
      div
        (h2
           ~a:[ a_class [ "heading-2"; "has-gap" ] ]
           [ txt (Utils.text_to_string language title) ]
         :: Page_contact_sessions.public_detail language sessions)
  in
  let registration_active sessions =
    match matches_filter with
    | false -> div ~a:[ a_class [ "gap" ] ] [ not_matching_warning language ]
    | true -> session_list sessions
  in
  let page_content =
    match upcoming_sessions, past_sessions, canceled_sessions with
    | [], [], [] ->
      Experiment.(
        (match
           experiment
           |> Public.direct_registration_disabled
           |> DirectRegistrationDisabled.value
         with
         | false -> [ registration_active grouped_sessions ]
         | true -> [ waiting_list_form () ]))
    | upcoming_sessions, past_sessions, canceled_sessions ->
      let open Pool_common.I18n in
      [ div
          ~a:[ a_class [ "stack-lg" ] ]
          [ sessions_html UpcomingSessionsTitle upcoming_sessions
          ; sessions_html PastSessionsTitle past_sessions
          ; sessions_html CanceledSessionsTitle canceled_sessions
          ]
      ]
  in
  page_content |> div ~a:[ a_class [ "gap-lg" ] ] |> experiment_detail_page experiment
;;

let show_online_study
      (experiment : Experiment.Public.t)
      matches_filter
      { Pool_context.language; _ }
      (argument :
        [> `Active of Time_window.t * Assignment.Public.t option
        | `Participated of Assignment.Public.t
        | `Upcoming of Time_window.t option
        ])
  =
  let html =
    let open Pool_common in
    let open Assignment in
    let start_button assignment =
      let field = Some Field.Survey in
      let control =
        let open Control in
        if CCOption.is_some assignment then Resume field else Start field
      in
      match matches_filter, assignment with
      | false, None -> not_matching_warning language
      | true, None | false, Some _ | true, Some _ ->
        div
          [ Component.Input.link_as_button
              ~control:(language, control)
              (HttpUtils.Url.Contact.experiment_path
                 ~id:(Experiment.Public.id experiment)
                 ~suffix:"start"
                 ())
          ]
    in
    let end_at_hint time_window =
      p
        ~a:[ a_class [ "gap-lg" ] ]
        [ strong
            [ txt
                (I18n.ExperimentOnlineParticipationDeadline
                   (Time_window.ends_at time_window)
                 |> Utils.text_to_string language)
            ]
        ]
    in
    let upcoming_hint time_window =
      let open Time_window in
      let hint =
        match time_window with
        | None -> I18n.ExperimentOnlineParticipationNoUpcoming
        | Some (time_window : t) ->
          I18n.ExperimentOnlineParticipationUpcoming
            (time_window.start |> Session.Start.value)
      in
      p [ strong [ txt (Utils.text_to_string language hint) ] ]
    in
    let participated_hint assignment =
      let open Utils in
      Component.Notification.notification
        language
        `Success
        [ p
            [ I18n.ExperimentOnlineParticiated
                (CreatedAt.value assignment.Public.created_at)
              |> text_to_string language
              |> txt
            ]
        ]
    in
    div ~a:[ a_class [ "gap"; "stack"; "flexcolumn" ] ]
    @@
    match argument with
    | `Active (time_window, assignment) ->
      [ start_button assignment; end_at_hint time_window ]
    | `Participated assignment -> [ participated_hint assignment ]
    | `Upcoming time_window -> [ upcoming_hint time_window ]
  in
  experiment_detail_page experiment html
;;

let online_study_completition (experiment : Experiment.Public.t) (_ : Pool_context.t) =
  div
    ~a:[ a_class [ "trim"; "measure"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1"; "has-gap"; "word-wrap-break" ] ]
        [ experiment |> experiment_title ]
    ; p [ txt "Thanks for participating" ]
    ]
;;

let list (list_context : experiment_list) Pool_context.{ language; _ } (experiments, query)
  =
  let page_language = language in
  let open Experiment in
  let suffix =
    match list_context with
    | `UpcomingOnsite -> "available-onsite"
    | `UpcomingOnline -> "available-online"
    | `Participated -> "participated"
  in
  let url = Http_utils.Url.Contact.experiment_path ~suffix () |> Uri.of_string in
  let data_table =
    let open Public in
    Component.DataTable.create_meta
      ~search:searchable_by
      ?filter:filterable_by
      url
      query
      page_language
  in
  let th_class = [ "w-12" ] in
  let cols = [ `column column_public_title; `empty ] in
  (* TODO: Make sure this shows "title" *)
  let row experiment =
    [ txt (Public.public_title experiment |> PublicTitle.value), Some Field.Title
    ; txt "btn", None (* TODO: Button *)
    ]
    |> CCList.map (fun (html, field) ->
      let label = Component.Table.data_label_opt page_language field in
      td ~a:label [ html ])
    |> tr
  in
  Component.DataTable.make
    ~break_mobile:true
    ~th_class
    ~target_id:"session-table"
    ~cols
    ~row
    data_table
    experiments
;;

let experiment_list title page_context context experiments =
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1 ~a:[ a_class [ "heading-1"; "has-gap" ] ] [ txt (I18n.content_to_string title) ]
    ; list page_context context experiments
    ]
;;
