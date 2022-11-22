open Tyxml.Html
open Component

let form_action ?path id =
  let base =
    Format.asprintf
      "/admin/experiments/%s/invitations"
      (id |> Pool_common.Id.value)
  in
  CCOption.map_or
    ~default:base
    (fun path -> Format.asprintf "%s/%s" base path)
    path
  |> Sihl.Web.externalize_path
;;

module Partials = struct
  let list Pool_context.{ csrf; language; _ } experiment invitation_list =
    let thead =
      Pool_common.Message.Field.
        [ Some contact; Some ResentAt; Some CreatedAt; None ]
    in
    let rows =
      CCList.map
        (fun (invitation : Invitation.t) ->
          let open Invitation in
          [ invitation.contact |> Contact.fullname |> txt
          ; invitation.resent_at
            |> CCOption.map_or ~default:"" (fun reset_at ->
                 reset_at
                 |> ResentAt.value
                 |> Pool_common.Utils.Time.formatted_date_time)
            |> txt
          ; invitation.created_at
            |> Pool_common.CreatedAt.value
            |> Pool_common.Utils.Time.formatted_date_time
            |> txt
          ; form
              ~a:
                [ a_method `Post
                ; a_action
                    (form_action
                       ~path:
                         (Format.asprintf
                            "%s/resend"
                            (invitation.Invitation.id |> Pool_common.Id.value))
                       experiment.Experiment.id)
                ; a_class [ "flexrow"; "justify-end" ]
                ]
              [ Input.csrf_element csrf ()
              ; Input.submit_element
                  language
                  Pool_common.Message.(Resend (Some Field.Invitation))
                  ()
              ]
          ])
        invitation_list
    in
    Table.horizontal_table `Striped language ~thead rows
  ;;

  let send_invitation
    Pool_context.{ csrf; language; _ }
    experiment
    key_list
    template_list
    filtered_contacts
    =
    let form_table =
      let rows =
        if CCList.is_empty filtered_contacts
        then p [ txt "No results found" ]
        else
          CCList.map
            (fun (contact : Contact.t) ->
              let id = Contact.id contact |> Pool_common.Id.value in
              [ div
                  [ input
                      ~a:
                        [ a_input_type `Checkbox
                        ; a_name
                            Pool_common.Message.Field.(Contacts |> array_key)
                        ; a_id id
                        ; a_value id
                        ]
                      ()
                  ; label
                      ~a:[ a_label_for id ]
                      [ txt (Contact.fullname contact) ]
                  ]
              ])
            filtered_contacts
          |> Table.horizontal_table `Striped language
      in
      div
        [ h4 ~a:[ a_class [ "heading-4" ] ] [ txt "Filtered contacts" ]; rows ]
    in
    div
      [ h3
          ~a:[ a_class [ "heading-3" ] ]
          [ txt
              Pool_common.(
                Message.(Send (Some Field.Invitation))
                |> Utils.control_to_string language)
          ]
      ; Filter.(
          filter_form
            csrf
            language
            (ExperimentParam experiment)
            key_list
            template_list)
      ; form
          ~a:
            [ a_method `Post
            ; a_action (form_action experiment.Experiment.id)
            ; a_class [ "stack" ]
            ]
          [ Input.csrf_element csrf ()
          ; form_table
          ; Input.submit_element
              language
              Pool_common.Message.(Send (Some Field.Invitation))
              ~submit_type:`Success
              ()
          ]
      ]
  ;;
end
