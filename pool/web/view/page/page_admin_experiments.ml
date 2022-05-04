open Tyxml.Html
open Component
module Message = Pool_common.Message

(* TODO [timhub]: create global nav component, when MR is merged *)
let subnav language id =
  let open Pool_common in
  I18n.[ Invitations, "/invitations" ]
  |> CCList.map (fun (label, url) ->
         li
           [ a
               ~a:
                 [ a_href
                     (Sihl.Web.externalize_path
                        (Format.asprintf
                           "/admin/experiments/%s/%s"
                           (Id.value id)
                           url))
                 ]
               [ txt (Utils.nav_link_to_string language label) ]
           ])
  |> ul
;;

let index experiment_list Pool_context.{ language; _ } =
  let experiment_item (experiment : Experiment.t) =
    let open Experiment in
    div
      ~a:[ a_class [ "flex-box"; "flex--row"; "flex--between" ] ]
      [ span [ txt (Title.value experiment.title) ]
      ; a
          ~a:
            [ a_href
                (Sihl.Web.externalize_path
                   (Format.asprintf
                      "/admin/experiments/%s"
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
    ; a
        ~a:[ a_href (Sihl.Web.externalize_path "/admin/experiments/new") ]
        [ txt
            Pool_common.(
              Message.(Add (Some Field.Experiment))
              |> Utils.control_to_string language)
        ]
    ]
;;

let form ?experiment Pool_context.{ language; csrf; _ } =
  let open Experiment in
  let action =
    match experiment with
    | None -> "/admin/experiments"
    | Some experiment ->
      Format.asprintf
        "/admin/experiments/%s"
        (experiment.id |> Pool_common.Id.value)
  in
  let value = CCFun.flip (CCOption.map_or ~default:"") experiment in
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
          ; a_action (Sihl.Web.externalize_path action)
          ; a_class [ "stack" ]
          ]
        [ Component.csrf_element csrf ()
        ; input_element
            language
            `Text
            Pool_common.Message.Field.Title
            (value title_value)
        ; input_element
            language
            `Text
            Pool_common.Message.Field.Description
            (value description_value)
        ; submit_element
            language
            Message.(
              let field = Some Field.Experiment in
              match experiment with
              | None -> Create field
              | Some _ -> Update field)
            ~classnames:[ "button--success" ]
            ()
        ]
    ]
;;

let detail experiment session_count Pool_context.{ language; _ } =
  let delete_form =
    match session_count > 0 with
    | true ->
      div
        [ submit_element
            language
            Message.(Delete (Some Field.Experiment))
            ~classnames:[ "button--neutral" ]
            ()
        ; p
            [ small
                [ txt
                    Pool_common.(
                      Message.ExperimenSessionCountNotZero
                      |> Utils.error_to_string language)
                ]
            ]
        ]
    | false ->
      Tyxml.Html.form
        ~a:
          [ a_method `Post
          ; a_action
              (Sihl.Web.externalize_path
                 (Format.asprintf
                    "/admin/experiments/%s/delete"
                    (experiment.Experiment.id |> Pool_common.Id.value)))
          ]
        [ submit_element
            language
            Message.(Delete (Some Field.Experiment))
            ~classnames:[ "button--failure" ]
            ()
        ]
  in
  let open Experiment in
  div
    ~a:[ a_class [ "stack" ] ]
    [ h1 [ txt (experiment.title |> Title.value) ]
    ; subnav language experiment.id
    ; p [ txt (experiment.description |> Description.value) ]
    ; p
        [ a
            ~a:
              [ a_href
                  (Sihl.Web.externalize_path
                     (Format.asprintf
                        "/admin/experiments/%s/edit"
                        (experiment.id |> Pool_common.Id.value)))
              ]
            [ txt
                Pool_common.(
                  Message.(Edit (Some Field.Experiment))
                  |> Utils.control_to_string language)
            ]
        ]
    ; delete_form
    ]
;;

let invitations
    (experiment_invitations : Experiment_type.invitations)
    filtered_subjects
    (Pool_context.{ language; _ } as context)
  =
  let experiment = experiment_invitations.Experiment_type.experiment in
  div
    [ subnav language experiment.Experiment.id
    ; h2
        [ txt
            Pool_common.(Utils.text_to_string language I18n.InvitationListTitle)
        ]
    ; Page_admin_invitations.Partials.list
        context
        experiment
        experiment_invitations.Experiment_type.invitations
    ; Page_admin_invitations.Partials.send_invitation
        context
        experiment
        filtered_subjects
    ]
;;
