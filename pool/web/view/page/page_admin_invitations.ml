open Tyxml.Html
open Component

let form_action ?path id =
  let base =
    Format.asprintf
      "/admin/experiments/%s/invitations"
      (id |> Experiment.Id.value)
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
      (Pool_common.Message.Field.[ Contact; ResentAt; Count; CreatedAt ]
       |> Table.fields_to_txt language)
      @ [ txt "" ]
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
          ; invitation.send_count |> SendCount.value |> CCInt.to_string |> txt
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
    Table.horizontal_table `Striped ~thead rows
  ;;

  let send_invitation
    Pool_context.{ csrf; language; _ }
    experiment
    key_list
    template_list
    query_experiments
    query_tags
    filtered_contacts
    =
    let filtered_contacts_form =
      match filtered_contacts with
      | None -> txt ""
      | Some filtered_contacts ->
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
            |> Table.horizontal_table `Striped
        in
        div
          [ h4
              ~a:[ a_class [ "heading-4" ] ]
              [ txt "Filtered contacts (development only)" ]
          ; form
              ~a:
                [ a_method `Post
                ; a_action (form_action experiment.Experiment.id)
                ; a_class [ "stack" ]
                ]
              [ Input.csrf_element csrf ()
              ; rows
              ; Input.submit_element
                  language
                  Pool_common.Message.(Send (Some Field.Invitation))
                  ~submit_type:`Success
                  ()
              ]
          ]
    in
    div
      [ h3
          ~a:[ a_class [ "heading-3" ] ]
          [ txt
              Pool_common.(
                Message.(Filter (Some Field.Contacts))
                |> Utils.control_to_string language)
          ]
      ; p
          [ txt Pool_common.(Utils.hint_to_string language I18n.FilterContacts)
          ]
      ; Filter.(
          filter_form
            csrf
            language
            (Http_utils.Filter.Experiment experiment)
            key_list
            template_list
            query_experiments
            query_tags)
      ; div ~a:[ a_class [ "gap-lg" ] ] [ filtered_contacts_form ]
      ]
  ;;

  let statistics language { Invitation.Statistics.total_sent; sent_by_count } =
    let open Pool_common in
    let to_string = CCInt.to_string in
    let field_to_string field =
      Utils.field_to_string language field |> CCString.capitalize_ascii
    in
    let thead =
      Message.Field.[ InvitationCount; ContactCount ]
      |> CCList.map CCFun.(field_to_string %> txt %> CCList.return %> th)
      |> tr
      |> CCList.return
      |> thead
    in
    let to_row ?(classnames = []) (key, value) =
      let td = td ~a:[ a_class classnames ] in
      tr [ td [ txt key ]; td [ txt value ] ]
    in
    let total =
      (field_to_string Message.Field.Total, to_string total_sent)
      |> to_row ~classnames:[ "font-bold" ]
    in
    div
      [ h3
          [ txt
              Pool_common.(
                Utils.text_to_string language I18n.InvitationsStatistic)
          ]
      ; (sent_by_count
         |> CCList.map (fun (key, value) ->
           (to_string key, to_string value) |> to_row)
         |> fun rows ->
         rows @ [ total ] |> table ~thead ~a:[ a_class [ "table"; "simple" ] ])
      ]
  ;;
end
