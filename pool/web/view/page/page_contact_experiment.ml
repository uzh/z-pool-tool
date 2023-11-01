open Tyxml.Html
open Component.Input
module PageSession = Page_contact_sessions
module Assignment = Page_contact_assignment
module HttpUtils = Http_utils
module Field = Pool_common.Message.Field

let index
  experiment_list
  upcoming_sessions
  waiting_list
  past_experiments
  custom_fields_ansered
  Pool_context.{ language; _ }
  =
  let list_html ?empty_msg title classnames list =
    div
      [ h2
          ~a:[ a_class [ "heading-2" ] ]
          [ txt Pool_common.(Utils.text_to_string language title) ]
      ; (match list, empty_msg with
         | [], Some empty_msg ->
           p Pool_common.[ Utils.text_to_string language empty_msg |> txt ]
         | [], None -> txt ""
         | list, _ -> div ~a:[ a_class classnames ] list)
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
                    Utils.text_to_string
                      language
                      I18n.DashboardProfileCompletionText)
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
  let experiment_description experiment =
    let open Experiment.Public in
    div
      ~a:[ a_class [ "flexrow"; "flex-gap"; "flexcolumn-mobile" ] ]
      [ experiment.description
        |> CCOption.map_or ~default:(txt "") (fun desc ->
          div
            ~a:[ a_class [ "grow" ] ]
            [ txt (Experiment.Description.value desc) ])
      ; div
          ~a:[ a_class [ "flexrow"; "align-end"; "justify-end" ] ]
          [ a
              ~a:
                [ a_href
                    (Sihl.Web.externalize_path
                       (Format.asprintf
                          "/experiments/%s"
                          (experiment.id |> Experiment.Id.value)))
                ]
              [ txt
                  Pool_common.(Message.More |> Utils.control_to_string language)
              ]
          ]
      ]
  in
  let experiment_item (experiment : Experiment.Public.t) =
    let open Experiment.Public in
    div
      ~a:[ a_class [ "stack-sm"; "inset-sm" ] ]
      [ p
          ~a:[ a_class [ "word-wrap-break" ] ]
          [ strong
              [ txt (Experiment.PublicTitle.value experiment.public_title) ]
          ]
      ; experiment_description experiment
      ]
  in
  let experiment_html =
    let open Pool_common.I18n in
    experiment_list
    |> CCList.map experiment_item
    |> list_html
         ExperimentListPublicTitle
         ~empty_msg:ExperimentListEmpty
         [ "striped" ]
  in
  let past_experiments_html =
    let open Pool_common.I18n in
    match past_experiments with
    | [] -> txt ""
    | past_experiments ->
      past_experiments
      |> CCList.map experiment_item
      |> list_html PastExperimentListPublicTitle [ "striped" ]
  in
  let session_html =
    let experiment_overview ((exp : Experiment.Public.t), parent, follow_ups) =
      let thead = Field.[ Some Start; Some Location ] in
      let session_item = PageSession.session_item `Upcoming language exp in
      let sessions = parent :: follow_ups in
      let row_formatter i =
        let open CCOption in
        i
        |> CCList.nth_opt sessions
        >>= fun s ->
        s.Session.Public.canceled_at >|= CCFun.const [ "bg-red-lighter" ]
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
        [ h3
            ~a:[ a_class [ "heading-4" ] ]
            [ txt Experiment.(exp.Public.public_title |> PublicTitle.value) ]
        ; div
            ~a:[ a_class [ "stack-sm" ] ]
            [ experiment_description exp; session_table ]
        ]
    in
    let open Pool_common.I18n in
    upcoming_sessions
    |> CCList.map experiment_overview
    |> list_html
         UpcomingSessionsTitle
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
      |> list_html
           ContactWaitingListTitle
           ~empty_msg:ContactWaitingListEmpty
           [ "striped" ]
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt Pool_common.(Utils.text_to_string language I18n.DashboardTitle) ]
    ; div
        ~a:[ a_class [ "stack-lg" ] ]
        [ notification
        ; div
            ~a:[ a_class [ "grid-col-2"; "gap-lg" ] ]
            [ div
                ~a:[ a_class [ "stack-lg" ] ]
                [ session_html; waiting_list_html ]
            ; experiment_html
            ; past_experiments_html
            ]
        ]
    ]
;;

let show
  (experiment : Experiment.Public.t)
  grouped_sessions
  upcoming_sessions
  past_sessions
  user_is_enlisted
  contact
  Pool_context.{ language; csrf; _ }
  =
  let open Experiment in
  let open Pool_common in
  let hint_to_string = Utils.hint_to_string language in
  let form_control, submit_class =
    match user_is_enlisted with
    | true -> Message.RemoveFromWaitingList, "error"
    | false -> Message.AddToWaitingList, "primary"
  in
  let session_list sessions =
    div
      ([ h2
           ~a:[ a_class [ "heading-2" ] ]
           [ txt (Utils.nav_link_to_string language I18n.Sessions) ]
       ; p [ txt (hint_to_string I18n.ExperimentSessionsPublic) ]
       ]
       @
       if CCList.is_empty sessions
       then
         [ p
             [ Utils.text_to_string language (I18n.EmtpyList Field.Sessions)
               |> txt
             ]
         ]
       else [ div [ PageSession.public_overview sessions experiment language ] ]
      )
  in
  let waiting_list_form () =
    let form_action =
      Format.asprintf
        "/experiments/%s/waiting-list/"
        (experiment.Public.id |> Experiment.Id.value)
      |> (fun url ->
           if user_is_enlisted then Format.asprintf "%s/remove" url else url)
      |> Sihl.Web.externalize_path
    in
    let text_blocks =
      let base =
        (if user_is_enlisted
         then I18n.ContactOnWaitingList
         else I18n.SignUpForWaitingList)
        |> fun msg -> p [ txt (hint_to_string msg) ]
      in
      let missing_phone =
        if CCOption.is_none contact.Contact.cell_phone
        then
          [ Component.Notification.notification
              ~link:("/user/contact-information", I18n.PersonalDetails)
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
          ~a:[ a_class [ "heading-2" ] ]
          [ txt (Utils.text_to_string language I18n.ExperimentWaitingListTitle)
          ]
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
           ~a:[ a_class [ "heading-2" ] ]
           [ txt (Utils.text_to_string language title) ]
         :: Page_contact_sessions.public_detail language sessions)
  in
  let html =
    match upcoming_sessions, past_sessions with
    | [], [] ->
      (match
         DirectRegistrationDisabled.value
           experiment.Public.direct_registration_disabled
       with
       | false -> session_list grouped_sessions
       | true -> div [ waiting_list_form () ])
    | upcoming_sessions, past_sessions ->
      let open Pool_common.I18n in
      div
        ~a:[ a_class [ "stack-lg" ] ]
        [ sessions_html UpcomingSessionsTitle upcoming_sessions
        ; sessions_html PastSessionsTitle past_sessions
        ]
  in
  div
    ~a:[ a_class [ "trim"; "measure"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1"; "word-wrap-break" ] ]
        [ txt (PublicTitle.value experiment.Public.public_title) ]
    ; div
        ~a:[ a_class [ "stack" ] ]
        [ experiment.Public.description
          |> CCOption.map_or ~default:(txt "") (fun desc ->
            p [ desc |> Description.value |> HttpUtils.add_line_breaks ])
        ; html
        ]
    ]
;;
