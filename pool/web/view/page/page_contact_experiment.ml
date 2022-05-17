open Tyxml.Html
open Component
module Session = Page_contact_sessions
module Assignment = Page_contact_assignment

let index experiment_list Pool_context.{ language; _ } =
  let experiment_item (experiment : Experiment_type.public) =
    let open Experiment_type in
    div
      ~a:[ a_class [ "flexrow"; "space-between"; "inset-sm"; "flex-gap" ] ]
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
                  Pool_common.(Message.More |> Utils.control_to_string language)
              ]
          ]
      ]
  in
  div
    ~a:[ a_class [ "trim"; "narrow"; "safety-margin" ] ]
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
    existing_assignment
    user_is_enlisted
    Pool_context.{ language; _ }
  =
  let open Experiment_type in
  let form_action =
    Format.asprintf
      "/experiments/%s/waiting-list/"
      (experiment.Experiment_type.id |> Pool_common.Id.value)
    |> (fun url ->
         if user_is_enlisted then Format.asprintf "%s/remove" url else url)
    |> Sihl.Web.externalize_path
  in
  let form_control, submit_class =
    match user_is_enlisted with
    | true -> Pool_common.Message.(RemoveFromWaitingList), "error"
    | false -> Pool_common.Message.(AddToWaitingList), "success"
  in
  let not_enrolled_html () =
    div
      [ h2 [ txt "Session" ]
      ; div [ Session.public_overview sessions experiment language ]
      ; div
          [ h2
              [ txt
                  Pool_common.(
                    Utils.text_to_string
                      language
                      I18n.ExperimentWaitingListTitle)
              ]
          ; form
              ~a:[ a_method `Post; a_action form_action ]
              [ submit_element
                  language
                  form_control
                  ~classnames:[ submit_class ]
                  ()
              ]
          ]
      ]
  in
  let enrolled_html existing_assignment =
    div
      [ p [ txt "You signed up for the following session: " ]
      ; Page_contact_sessions.public_detail
          existing_assignment.Assignment_type.session
          language
      ]
  in
  let html =
    match existing_assignment with
    | Some assignment -> enrolled_html assignment
    | None -> not_enrolled_html ()
  in
  div
    ~a:[ a_class [ "trim"; "narrow"; "safety-margin" ] ]
    [ div
        ~a:[ a_class [ "stack" ] ]
        [ p [ txt (Experiment.Description.value experiment.description) ]
        ; h2
            ~a:[ a_class [ "heading-2" ] ]
            [ txt
                Pool_common.(
                  Utils.text_to_string language I18n.ExperimentWaitingListTitle)
            ]
        ; html
        ]
    ]
;;
