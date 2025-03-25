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
  let experiment_link id =
    div
      [ a
          ~a:
            [ a_href
                (HttpUtils.Url.Contact.experiment_path ~id ()
                 |> HttpUtils.externalize_path_with_params query_parameters)
            ]
          [ Control.More |> Pool_common.Utils.control_to_string language |> txt ]
      ]
  in
  let experiment_title exp =
    div
      ~a:[ a_class [ "flexrow"; "flex-gap"; "justify-between"; "align-center" ] ]
      [ div
          ~a:[ a_class [ "grow" ] ]
          [ h4 ~a:[ a_class [ "word-wrap-break" ] ] [ strong [ exp |> experiment_title ] ]
          ]
      ; exp |> Experiment.Public.id |> experiment_link
      ]
  in
  let experiment_item exp =
    div
      ~a:[ a_class [ "flexcolumn"; "stack-sm"; "inset-sm" ] ]
      [ experiment_title exp; experiment_public_description exp ]
  in
  let open Pool_common.I18n in
  let experiment_html =
    experiment_list
    |> CCList.map experiment_item
    |> list_html
         i18n.experiment_registration
         ~note:ExperimentSessionsPublic
         ~empty_msg:ExperimentListEmpty
         [ "striped" ]
  in
  let online_studies_html =
    online_studies
    |> CCList.map experiment_item
    |> list_html i18n.online_studies ~empty_msg:ExperimentOnlineListEmpty [ "striped" ]
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
    let upcoming_session_list ((exp : Experiment.Public.t), parent, follow_ups) =
      let open Session in
      let thead = Field.[ Some Start; Some Location ] in
      let session_item session =
        [ div
            ((if CCOption.is_some session.Public.canceled_at
              then
                [ strong [ txt Pool_common.(Utils.text_to_string language I18n.Canceled) ]
                ; br ()
                ]
              else [])
             @ [ txt (Session.Public.start_end_with_duration_human session) ])
        ; session.Public.location |> Component.Location.preview
        ]
      in
      let sessions = parent :: follow_ups in
      let row_formatter i =
        let open CCOption in
        i
        |> CCList.nth_opt sessions
        >>= fun s -> s.Session.Public.canceled_at >|= CCFun.const [ "bg-red-lighter" ]
      in
      let session_table =
        sessions
        |> CCList.map session_item
        |> Component.Table.responsive_horizontal_table
             `Striped
             language
             ~align_last_end:true
             ~align_top:true
             ~row_formatter
             thead
      in
      div
        ~a:[ a_class [ "flexcolumn"; "stack" ] ]
        [ div
            ~a:[ a_class [ "stack-sm" ] ]
            [ experiment_title exp; experiment_public_description exp ]
        ; session_table
        ]
    in
    let open Pool_common.I18n in
    upcoming_sessions
    |> CCList.map upcoming_session_list
    |> list_html
         i18n.upcoming_sessions
         ~empty_msg:UpcomingSessionsListEmpty
         [ "stack-lg" ]
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
            ~a:[ a_class [ "grid-col-2"; "grid-gap-lg"; "gap-lg" ] ]
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
      ([ h2
           ~a:[ a_class [ "heading-2"; "has-gap" ] ]
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
  let html =
    match upcoming_sessions, past_sessions, canceled_sessions with
    | [], [], [] ->
      Experiment.(
        (match
           experiment
           |> Public.direct_registration_disabled
           |> DirectRegistrationDisabled.value
         with
         | false -> registration_active grouped_sessions
         | true -> div [ waiting_list_form () ]))
    | upcoming_sessions, past_sessions, canceled_sessions ->
      let open Pool_common.I18n in
      div
        ~a:[ a_class [ "gap-lg"; "stack-lg" ] ]
        [ sessions_html UpcomingSessionsTitle upcoming_sessions
        ; sessions_html PastSessionsTitle past_sessions
        ; sessions_html CanceledSessionsTitle canceled_sessions
        ]
  in
  experiment_detail_page experiment html
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
