open CCFun
open Tyxml.Html
open Component.Input
open Pool_message
module PageSession = Page_contact_sessions
module HttpUtils = Http_utils

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
       (PublicDescription.value %> HttpUtils.add_line_breaks)
;;

let experiment_title =
  let open Experiment in
  Public.public_title %> PublicTitle.value %> txt
;;

let not_matching_warning language =
  Component.Notification.create
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

let make_panel ?(classnames = []) ~query_parameters url content =
  a
    ~a:
      [ a_href (HttpUtils.externalize_path_with_params query_parameters url)
      ; a_class ([ "panel"; "flexrow"; "flex-gap"; "inset-sm" ] @ classnames)
      ]
    [ div ~a:[ a_class [ "grow" ] ] content; Component.Icon.(to_html ChevronForward) ]
;;

module Detail = struct
  open Pool_common

  let waiting_list_html
        Pool_context.{ language; query_parameters; csrf; _ }
        experiment
        contact
        user_is_enlisted
    =
    let form_control, submit_class =
      match user_is_enlisted with
      | true -> Control.RemoveFromWaitingList, "error"
      | false -> Control.AddToWaitingList, "primary"
    in
    let hint_to_string = Utils.hint_to_string language in
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
          [ Component.Notification.create
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
      div ~a:[ a_class [ "stack" ] ] (missing_phone @ [ base ])
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
  ;;

  let session_list { Pool_context.language; _ } experiment sessions =
    div
      ~a:[ a_class [ "stack" ] ]
      ([ h2
           ~a:[ a_class [ "heading-2" ] ]
           [ txt (Utils.nav_link_to_string language I18n.Sessions) ]
       ; p [ txt (Utils.hint_to_string language I18n.ExperimentSessionsPublic) ]
       ]
       @
       if CCList.is_empty sessions
       then
         [ p
             ~a:[ a_class [ "gap" ] ]
             [ Utils.text_to_string language (I18n.EmtpyList Field.Sessions) |> txt ]
         ]
       else [ div [ PageSession.public_overview sessions experiment language ] ])
  ;;

  let onsite_study
        experiment
        matches_filter
        grouped_sessions
        upcoming_sessions
        past_sessions
        canceled_sessions
        user_is_enlisted
        contact
        (Pool_context.{ language; _ } as context)
    =
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
      | true -> session_list context experiment sessions
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
           | true -> [ waiting_list_html context experiment contact user_is_enlisted ]))
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

  let online_study
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
        Component.Notification.create
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
end

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
  let cols = [ `column Public.column_public_title; `empty ] in
  let row experiment =
    [ txt (Public.public_title experiment |> PublicTitle.value), Some Field.Title
    ; ( Component.Input.link_as_button
          ~icon:Icon.Eye
          (HttpUtils.Url.Contact.experiment_path ~id:(Public.id experiment) ())
      , None )
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

module History = struct
  let list Pool_context.{ language; _ } (experiments, query) =
    let page_language = language in
    let open Experiment in
    let open Component in
    let url =
      Http_utils.Url.Contact.experiment_path ~suffix:"history" () |> Uri.of_string
    in
    let data_table =
      let open Public in
      DataTable.create_meta
        ~search:searchable_by
        ?filter:filterable_by
        url
        query
        page_language
    in
    let th_class = [ "w-12" ] in
    let cols = [ `column Public.column_public_title; `empty ] in
    let row (experiment, pending) =
      let status =
        match pending with
        | false -> txt ""
        | true -> span ~a:[ a_class [ "tag"; "primary"; "inline" ] ] [ txt "pending" ]
      in
      [ ( span
            ~a:[ a_class [ "flexrow"; "flex-gap"; "align-center" ] ]
            [ span [ txt (public_title experiment |> PublicTitle.value) ]; status ]
        , Some Field.Title )
      ; ( Input.link_as_button
            ~icon:Icon.Eye
            (HttpUtils.Url.Contact.experiment_path ~id:(id experiment) ())
        , None )
      ]
      |> CCList.map (fun (html, field) ->
        let label = Table.data_label_opt page_language field in
        td ~a:label [ html ])
      |> tr
    in
    DataTable.make
      ~break_mobile:true
      ~th_class
      ~target_id:"experiment-history-table"
      ~cols
      ~row
      data_table
      experiments
  ;;

  let show title context experiments =
    div
      ~a:[ a_class [ "trim"; "safety-margin" ] ]
      [ h1
          ~a:[ a_class [ "heading-1"; "has-gap" ] ]
          [ txt (I18n.content_to_string title) ]
      ; list context experiments
      ]
  ;;
end
