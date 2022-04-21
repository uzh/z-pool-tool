open Tyxml.Html
open Component
module Message = Pool_common.Message

let index experiment_list Pool_context.{ language; _ } =
  let experiment_item (experiment : Experiment.t) =
    let open Experiment in
    div
      ~a:[ a_class [ "flex-box"; "flex--row"; "flex--between" ] ]
      [ span [ txt (Title.value experiment.title) ]
      ; a
          ~a:[ a_href "#" ]
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
    ; a
        ~a:[ a_href (Sihl.Web.externalize_path "/admin/experiments/new") ]
        [ txt
            Pool_common.(
              Message.(Add (Some Field.Experiment))
              |> Utils.control_to_string language)
        ]
    ]
;;

let new_form csrf Pool_context.{ language; _ } =
  div
    ~a:[ a_class [ "stack" ] ]
    [ h1
        [ txt
            Pool_common.(
              Utils.text_to_string Language.En I18n.ExperimentNewTitle)
        ]
    ; form
        ~a:
          [ a_method `Post
          ; a_action (Sihl.Web.externalize_path "/admin/experiments")
          ; a_class [ "stack" ]
          ]
        [ Component.csrf_element csrf ()
        ; input_element language `Text Pool_common.Message.Field.Title ""
        ; input_element language `Text Pool_common.Message.Field.Description ""
        ; submit_element
            language
            Message.(Create (Some Field.Experiment))
            ~classnames:[ "button--success" ]
            ()
        ]
    ]
;;
