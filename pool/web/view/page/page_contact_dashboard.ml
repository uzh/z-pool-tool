open CCFun.Infix
open Tyxml.Html
open Pool_message
module PageSession = Page_contact_sessions
module HttpUtils = Http_utils

module PageContactExperiment = struct
  let total_link_from_query
        url
        text
        { Query.pagination; _ }
        Pool_context.{ language; query_parameters; _ }
    =
    let open Query in
    let open CCOption in
    let make_total total =
      match total with
      | None -> txt ""
      | Some total -> span ~a:[ a_class [ "counter" ] ] [ txt (string_of_int total) ]
    in
    let total =
      pagination >|= (fun { Pagination.item_count; _ } -> item_count) |> make_total
    in
    let html =
      [ span
          ~a:[ a_class [ "flexrow"; "flex-gap"; "align-center" ] ]
          [ total; txt Pool_common.(Utils.control_to_string language text) ]
      ]
    in
    let panel =
      a
        ~a:
          [ a_href (HttpUtils.externalize_path_with_params query_parameters url)
          ; a_class [ "panel"; "flexrow"; "flex-gap"; "inset-sm" ]
          ]
        [ div ~a:[ a_class [ "grow" ] ] html; Component.Icon.(to_html ChevronForward) ]
    in
    panel
  ;;

  let list_html ?empty_msg ?note language title classnames list =
    div
      [ h2
          ~a:[ a_class [ "heading-2"; "has-gap" ] ]
          [ txt (I18n.content_to_string title) ]
      ; div
          ~a:[ a_class [ "stack" ] ]
          [ CCOption.map_or
              ~default:(txt "")
              (Pool_common.Utils.hint_to_string language %> txt %> CCList.return %> p)
              note
          ; (match list, empty_msg with
             | [], Some empty_msg ->
               p Pool_common.[ Utils.text_to_string language empty_msg |> txt ]
             | [], None -> txt ""
             | list, _ -> div ~a:[ a_class classnames ] list)
          ]
      ]
  ;;
end

let notification custom_fields_answered language =
  let custom_fields_ansered =
    if custom_fields_answered
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
        (Component.Notification.create
           ~link:("/user/personal-details", Pool_common.I18n.PersonalDetails)
           language
           `Warning
           text))
  in
  [ custom_fields_ansered ]
  |> CCList.filter_map CCFun.id
  |> div ~a:[ a_class [ "stack" ] ]
;;

let create Pool_context.{ language; _ } dashboard_intro custom_fields_answered content =
  let intro =
    div
      ~a:[ a_class [ "rich-text" ] ]
      [ dashboard_intro |> I18n.content_to_string |> Unsafe.data ]
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1"; "has-gap" ] ]
        [ txt Pool_common.(Utils.text_to_string language I18n.DashboardTitle) ]
    ; div
        ~a:[ a_class [ "stack-lg" ] ]
        [ notification custom_fields_answered language
        ; div
            ~a:[ a_class [ "grid-col-2"; "grid-gap-lg"; "grid-row-gap-xl"; "gap-lg" ] ]
            (intro :: content)
        ]
    ]
;;

let create_profile_only context dashboard_intro custom_fields_answered =
  create context dashboard_intro custom_fields_answered []
;;

let prepare_content
      i18n
      experiment_list
      online_studies
      upcoming_sessions
      waiting_list
      (Pool_context.{ language; query_parameters; _ } as context)
  =
  let find_key key = I18n.extract_by_key_exn i18n key language in
  let experiment_item experiment =
    let open Experiment in
    let html =
      [ div
          ~a:[ a_class [ "flexcolumn"; "stack-xs" ] ]
          [ h4
              ~a:[ a_class [ "word-wrap-break" ] ]
              [ txt (experiment |> Public.public_title |> PublicTitle.value) ]
          ; div
              ~a:[ a_class [ "truncate-6"; "hide-empty" ] ]
              [ Page_contact_experiment.experiment_public_description experiment ]
          ]
      ]
    in
    let url = HttpUtils.Url.Contact.experiment_path ~id:(Public.id experiment) () in
    let panel =
      a
        ~a:
          [ a_href (HttpUtils.externalize_path_with_params query_parameters url)
          ; a_class [ "panel"; "flexrow"; "flex-gap"; "inset-sm" ]
          ]
        [ div ~a:[ a_class [ "grow" ] ] html; Component.Icon.(to_html ChevronForward) ]
    in
    panel
  in
  let open Pool_common.I18n in
  let experiment_html =
    let experiments, query = experiment_list in
    let total =
      PageContactExperiment.total_link_from_query
        (Page_contact_experiment.experiment_list_url `UpcomingOnsite)
        Control.AllAvailableExperiments
        query
        context
    in
    let panel =
      let sessions = CCList.map experiment_item experiments in
      PageContactExperiment.list_html
        ~note:ExperimentSessionsPublic
        ~empty_msg:ExperimentListEmpty
        language
        (find_key I18n.Key.DashboardExperimentRegistration)
        [ "panel-list" ]
        (sessions @ [ total ])
    in
    div [ panel ]
  in
  let online_studies_html =
    let experiments, query = online_studies in
    let total =
      PageContactExperiment.total_link_from_query
        (Page_contact_experiment.experiment_list_url `UpcomingOnline)
        Control.AllAvailableExperiments
        query
        context
    in
    let panel =
      let sessions = CCList.map experiment_item experiments in
      PageContactExperiment.list_html
        ~empty_msg:ExperimentOnlineListEmpty
        language
        (find_key I18n.Key.DashboardOnlineStudies)
        [ "panel-list" ]
        (sessions @ [ total ])
    in
    div [ panel ]
  in
  let past_experiments_html =
    let url =
      HttpUtils.(
        Url.Contact.experiment_path ~suffix:"history" ()
        |> externalize_path_with_params query_parameters)
    in
    PageContactExperiment.list_html
      language
      (find_key I18n.Key.DashboardExperimentHistory)
      []
      [ a
          ~a:[ a_href url ]
          [ txt Pool_common.(Utils.hint_to_string language I18n.ContactExperimentHistory)
          ]
      ]
  in
  let session_html =
    let upcoming_session { Session.Public.experiment_id; start; location; canceled_at; _ }
      =
      let classnames, tags =
        match canceled_at with
        | None -> [], []
        | Some _ ->
          let tag =
            Component.Tag.create_chip
              ~inline:true
              ~style:`Error
              Pool_common.(Utils.text_to_string language I18n.Canceled)
          in
          [ "bg-red-lighter" ], [ tag ]
      in
      let html =
        [ div
            ~a:[ a_class [ "flexcolumn"; "stack-xs" ] ]
            [ p
                ~a:[ a_class [ "flexrow"; "flex-gap"; "align-center" ] ]
                (span
                   [ txt (Session.Start.value start |> Pool_model.Time.formatted_date_time)
                   ]
                 :: tags)
            ; p [ txt (location.Pool_location.name |> Pool_location.Name.value) ]
            ]
        ]
      in
      let url = HttpUtils.Url.Contact.experiment_path ~id:experiment_id () in
      let panel =
        a
          ~a:
            [ a_href (HttpUtils.externalize_path_with_params query_parameters url)
            ; a_class ([ "panel"; "flexrow"; "flex-gap"; "inset-sm" ] @ classnames)
            ]
          [ div ~a:[ a_class [ "grow" ] ] html; Component.Icon.(to_html ChevronForward) ]
      in
      panel
    in
    let sessions, query = upcoming_sessions in
    let total =
      PageContactExperiment.total_link_from_query
        (HttpUtils.Url.Contact.session_path ())
        Control.AllSessions
        query
        context
    in
    let panel =
      let sessions = CCList.map upcoming_session sessions in
      PageContactExperiment.list_html
        ~empty_msg:UpcomingSessionsListEmpty
        language
        (find_key I18n.Key.DashboardUpcomingSessions)
        [ "panel-list" ]
        (sessions @ [ total ])
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
      |> PageContactExperiment.list_html
           ~empty_msg:ContactWaitingListEmpty
           language
           (find_key I18n.Key.DashboardWaitingList)
           [ "panel-list" ]
  in
  [ session_html
  ; experiment_html
  ; online_studies_html
  ; waiting_list_html
  ; past_experiments_html
  ]
;;
