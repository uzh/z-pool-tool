open Tyxml.Html
open Component
module Session = Page_contact_sessions
module Assignment = Page_contact_assignment

let index experiment_list Pool_context.{ language; _ } =
  let experiment_item (experiment : Experiment.Public.t) =
    let open Experiment.Public in
    div
      ~a:[ a_class [ "stack-sm"; "inset-sm" ] ]
      [ p
          [ strong
              [ txt (Experiment.PublicTitle.value experiment.public_title) ]
          ]
      ; div
          ~a:[ a_class [ "flexrow"; "space-between"; "flex-gap" ] ]
          [ div
              ~a:[ a_class [ "grow" ] ]
              [ txt (Experiment.Description.value experiment.description) ]
          ; div
              [ a
                  ~a:
                    [ a_href
                        (Sihl.Web.externalize_path
                           (Format.asprintf
                              "/experiments/%s"
                              (experiment.id |> Pool_common.Id.value)))
                    ]
                  [ txt
                      Pool_common.(
                        Message.More |> Utils.control_to_string language)
                  ]
              ]
          ]
      ]
  in
  div
    ~a:[ a_class [ "trim"; "measure"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt
            Pool_common.(Utils.text_to_string language I18n.ExperimentListTitle)
        ]
    ; div
        ~a:[ a_class [ "striped" ] ]
        (CCList.map experiment_item experiment_list)
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
  let form_action =
    Format.asprintf
      "/experiments/%s/waiting-list/"
      (experiment.id |> Pool_common.Id.value)
    |> (fun url ->
         if user_is_enlisted then Format.asprintf "%s/remove" url else url)
    |> Sihl.Web.externalize_path
  in
  let form_control, submit_class =
    match user_is_enlisted with
    | true -> Pool_common.Message.(RemoveFromWaitingList), "error"
    | false -> Pool_common.Message.(AddToWaitingList), "success"
  in
  let session_list sessions =
    div
      ~a:[ a_class [ "stack-lg" ] ]
      [ h2
          ~a:[ a_class [ "heading-2" ] ]
          [ txt Pool_common.(Utils.nav_link_to_string language I18n.Sessions) ]
      ; div [ Session.public_overview sessions experiment language ]
      ]
  in
  let waiting_list_form () =
    div
      ~a:[ a_class [ "stack" ] ]
      [ h2
          ~a:[ a_class [ "heading-2" ] ]
          [ txt
              Pool_common.(
                Utils.text_to_string language I18n.ExperimentWaitingListTitle)
          ]
      ; (if user_is_enlisted
        then div []
        else
          p
            [ txt
                Pool_common.(
                  Utils.hint_to_string language I18n.SignUpForWaitingList)
            ])
      ; form
          ~a:[ a_method `Post; a_action form_action ]
          [ csrf_element csrf ()
          ; submit_element language form_control ~classnames:[ submit_class ] ()
          ]
      ]
  in
  let enrolled_html session =
    div
      [ p
          [ txt
              Pool_common.(
                Utils.text_to_string language I18n.ExperimentContactEnrolledNote)
          ]
      ; Page_contact_sessions.public_detail session language
      ]
  in
  let html =
    match session_user_is_assigned with
    | Some session -> enrolled_html session
    | None ->
      (match
         Experiment.DirectRegistrationDisabled.value
           experiment.direct_registration_disabled
       with
      (* TODO[timhub]: check functionality *)
      | false -> session_list sessions
      | true -> div [ waiting_list_form () ])
  in
  div
    ~a:[ a_class [ "trim"; "measure"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
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
