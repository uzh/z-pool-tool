open Tyxml.Html
open Component

module Partials = struct
  let list invitation_list =
    div
      ~a:[ a_class [ "stack" ] ]
      (CCList.map
         (fun (invitation : Invitation.t) ->
           let open Invitation in
           div
             ~a:[ a_class [ "flex-box"; "flex--row"; "flex--between" ] ]
             [ span [ invitation.participant |> Participant.fullname |> txt ]
             ; span
                 [ invitation.created_at
                   |> Pool_common.CreatedAt.value
                   |> Pool_common.Utils.Time.formatted_date_time
                   |> txt
                 ]
             ])
         invitation_list)
  ;;

  let send_invitation experiment language filtered_participants =
    div
      [ h3
          [ txt
              Pool_common.(
                I18n.InvitationNewTitle |> Utils.text_to_string language)
          ]
      ; form
          ~a:
            [ a_method `Post
            ; a_action
                (Format.asprintf
                   "/admin/experiments/%s/invitations"
                   (experiment.Experiment.id |> Pool_common.Id.value)
                |> Sihl.Web.externalize_path)
            ]
          [ div
              ~a:[ a_class [ "stack" ] ]
              (CCList.map
                 (fun (participant : Participant.t) ->
                   let id =
                     Participant.id participant |> Pool_common.Id.value
                   in
                   div
                     ~a:[ a_class [ "is-box"; "flex--row" ] ]
                     [ input
                         ~a:
                           [ a_input_type `Checkbox
                           ; a_name "participants[]"
                           ; a_id id
                           ; a_value id
                           ]
                         ()
                     ; label
                         ~a:[ a_label_for id ]
                         [ txt (Participant.fullname participant) ]
                     ])
                 filtered_participants)
          ; submit_element
              language
              Pool_common.Message.(Send (Some Field.Invitation))
              ~classnames:[ "button--success" ]
              ()
          ]
      ]
  ;;
end
