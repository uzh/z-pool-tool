open Tyxml.Html

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

let show (experiment : Experiment_type.public) _ =
  let open Experiment_type in
  div [ txt (Experiment.Description.value experiment.description) ]
;;
