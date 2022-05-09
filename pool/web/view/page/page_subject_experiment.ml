open Tyxml.Html
open Component

let index experiment_list Pool_context.{ language; _ } =
  let experiment_item (experiment : Experiment_type.public) =
    let open Experiment_type in
    div
      ~a:[ a_class [ "flex-box"; "flex--row"; "flex--between" ] ]
      [ span [ txt (Experiment.Description.value experiment.description) ]
      ; a
          ~a:
            [ a_href
                (Sihl.Web.externalize_path
                   (Format.asprintf
                      "/experiments/%s"
                      (experiment.id |> Pool_common.Id.value)))
            ]
          [ txt Pool_common.(Message.More |> Utils.control_to_string language) ]
      ]
  in
  div
    ~a:[ a_class [ "stack" ] ]
    [ h1
        [ txt
            Pool_common.(Utils.text_to_string language I18n.ExperimentListTitle)
        ]
    ; div
        ~a:[ a_class [ "stack" ] ]
        (CCList.map experiment_item experiment_list)
    ]
;;

let show experiment user_is_enlisted Pool_context.{ language; _ } =
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
    | true -> Pool_common.Message.(RemoveFromWaitingList), "button-error"
    | false -> Pool_common.Message.(AddToWaitingList), "button--success"
  in
  div
    [ div [ txt (Experiment.Description.value experiment.description) ]
    ; div
        [ h2
            [ txt
                Pool_common.(
                  Utils.text_to_string language I18n.ExperimentWaitingListTitle)
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
;;
