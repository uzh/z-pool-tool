open Tyxml.Html
open Component.Input
module Session = Page_contact_sessions
module Assignment = Page_contact_assignment
module HttpUtils = Http_utils

let index experiment_list upcoming_sessions Pool_context.{ language; _ } =
  let list_html title empty classnames list =
    div
      [ h2
          ~a:[ a_class [ "heading-2" ] ]
          [ txt Pool_common.(Utils.text_to_string language title) ]
      ; (if CCList.is_empty list
        then p Pool_common.[ Utils.text_to_string language empty |> txt ]
        else div ~a:[ a_class classnames ] list)
      ]
  in
  let experiment_html =
    let experiment_item (experiment : Experiment.Public.t) =
      let open Experiment.Public in
      div
        ~a:[ a_class [ "stack-sm"; "inset-sm" ] ]
        [ p
            ~a:[ a_class [ "word-wrap-break" ] ]
            [ strong
                [ txt (Experiment.PublicTitle.value experiment.public_title) ]
            ]
        ; div
            ~a:[ a_class [ "flexrow"; "flex-gap"; "flexcolumn-mobile" ] ]
            [ div
                ~a:[ a_class [ "grow" ] ]
                [ txt (Experiment.Description.value experiment.description) ]
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
                        Pool_common.(
                          Message.More |> Utils.control_to_string language)
                    ]
                ]
            ]
        ]
    in
    let open Pool_common.I18n in
    experiment_list
    |> CCList.map experiment_item
    |> list_html ExperimentListPublicTitle ExperimentListEmpty [ "striped" ]
  in
  let session_html =
    let experiment_overview (exp, parent, follow_ups) =
      let thead = Field.[ Some Start; Some Duration; Some Location ] in
      let session_item = Session.session_item `Upcoming language exp in
      let session_table =
        session_item parent :: CCList.map session_item follow_ups
        |> Component.Table.responsive_horizontal_table
             `Striped
             language
             ~align_last_end:true
             ~align_top:true
             thead
      in
      div
        [ h3
            ~a:[ a_class [ "heading-4" ] ]
            [ txt Experiment.(exp.Public.public_title |> PublicTitle.value) ]
        ; session_table
        ]
    in
    let open Pool_common.I18n in
    upcoming_sessions
    |> CCList.map experiment_overview
    |> list_html UpcomingSessionsTitle UpcomingSessionsListEmpty [ "stack-lg" ]
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1 ~a:[ a_class [ "heading-1" ] ] [ txt "Dashboard TODO" ]
    ; div
        ~a:[ a_class [ "switcher"; "flex-gap-lg" ] ]
        [ experiment_html; session_html ]
    ]
;;

let show
  experiment
  sessions
  session_user_is_assigned
  user_is_enlisted
  Pool_context.{ language; csrf; _ }
  =
  let open Experiment.Public in
  let open Pool_common in
  let form_action =
    Format.asprintf
      "/experiments/%s/waiting-list/"
      (experiment.id |> Experiment.Id.value)
    |> (fun url ->
         if user_is_enlisted then Format.asprintf "%s/remove" url else url)
    |> Sihl.Web.externalize_path
  in
  let form_control, submit_class =
    match user_is_enlisted with
    | true -> Message.RemoveFromWaitingList, "error"
    | false -> Message.AddToWaitingList, "primary"
  in
  let session_list sessions =
    if CCList.is_empty sessions
    then
      p
        [ Utils.text_to_string language (I18n.EmtpyList Message.Field.Sessions)
          |> txt
        ]
    else
      div
        [ h2
            ~a:[ a_class [ "heading-2" ] ]
            [ txt (Utils.nav_link_to_string language I18n.Sessions) ]
        ; p
            [ txt (Utils.hint_to_string language I18n.ExperimentSessionsPublic)
            ]
        ; div [ Session.public_overview sessions experiment language ]
        ]
  in
  let waiting_list_form () =
    div
      ~a:[ a_class [ "stack" ] ]
      [ h2
          ~a:[ a_class [ "heading-2" ] ]
          [ txt (Utils.text_to_string language I18n.ExperimentWaitingListTitle)
          ]
      ; (if user_is_enlisted
        then p [ txt (Utils.hint_to_string language I18n.ContactOnWaitingList) ]
        else p [ txt (Utils.hint_to_string language I18n.SignUpForWaitingList) ])
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
  let enrolled_html sessions =
    div
      (p
         [ txt
             (Utils.text_to_string language I18n.ExperimentContactEnrolledNote)
         ]
      :: Page_contact_sessions.public_detail language sessions)
  in
  let html =
    match session_user_is_assigned with
    | [] ->
      (match
         Experiment.DirectRegistrationDisabled.value
           experiment.direct_registration_disabled
       with
       | false -> session_list sessions
       | true -> div [ waiting_list_form () ])
    | sessions -> enrolled_html sessions
  in
  div
    ~a:[ a_class [ "trim"; "measure"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1"; "word-wrap-break" ] ]
        [ txt (Experiment.PublicTitle.value experiment.public_title) ]
    ; div
        ~a:[ a_class [ "stack" ] ]
        [ p
            [ Experiment.Description.value experiment.description
              |> HttpUtils.add_line_breaks
            ]
        ; html
        ]
    ]
;;
