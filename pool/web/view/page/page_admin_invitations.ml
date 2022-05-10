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
    let html_body =
      CCList.map
        (fun (invitation : Invitation.t) ->
          let open Invitation in
          tr
            [ td [ invitation.contact |> Contact.fullname |> txt ]
            ; td
                [ invitation.resent_at
                  |> CCOption.map_or ~default:"" (fun reset_at ->
                         reset_at
                         |> ResentAt.value
                         |> Pool_common.Utils.Time.formatted_date_time)
                  |> txt
                ]
            ; td
                [ invitation.created_at
                  |> Pool_common.CreatedAt.value
                  |> Pool_common.Utils.Time.formatted_date_time
                  |> txt
                ]
            ; td
                [ form
                    ~a:
                      [ a_method `Post
                      ; a_action
                          (form_action
                             ~path:
                               (Format.asprintf
                                  "%s/resend"
                                  (invitation.Invitation.id
                                  |> Pool_common.Id.value))
                             experiment.Experiment.id)
                      ]
                    [ csrf_element csrf ()
                    ; submit_element
                        language
                        Pool_common.Message.(Resend (Some Field.Invitation))
                        ~classnames:[ "button--warning" ]
                        ()
                    ]
                ]
            ])
        invitation_list
    in
    let thead =
      CCList.map
        (fun field ->
          th
            [ txt
                (field
                |> CCOption.map_or ~default:"" (fun field ->
                       Pool_common.Utils.field_to_string language field))
            ])
        Pool_common.Message.Field.
          [ Some contact; Some ResentAt; Some CreatedAt; None ]
      |> tr
      |> CCList.pure
      |> thead
    in
    table ~thead html_body
  ;;

  let send_invitation
      Pool_context.{ csrf; language; _ }
      experiment
      filtered_contacts
    =
    div
      [ h3
          [ txt
              Pool_common.(
                I18n.InvitationNewTitle |> Utils.text_to_string language)
          ]
      ; form
          ~a:[ a_method `Post; a_action (form_action experiment.Experiment.id) ]
          [ csrf_element csrf ()
          ; div
              ~a:[ a_class [ "stack" ] ]
              (CCList.map
                 (fun (contact : Contact.t) ->
                   let id = Contact.id contact |> Pool_common.Id.value in
                   div
                     ~a:[ a_class [ "is-box"; "flex--row" ] ]
                     [ input
                         ~a:
                           [ a_input_type `Checkbox
                           ; a_name "contacts[]"
                           ; a_id id
                           ; a_value id
                           ]
                         ()
                     ; label
                         ~a:[ a_label_for id ]
                         [ txt (Contact.fullname contact) ]
                     ])
                 filtered_contacts)
          ; submit_element
              language
              Pool_common.Message.(Send (Some Field.Invitation))
              ~classnames:[ "button--success" ]
              ()
          ]
      ]
  ;;
end
