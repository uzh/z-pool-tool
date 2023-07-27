open Tyxml.Html
open Component
open Pool_common

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
      (Message.Field.[ Contact; ResentAt; CreatedAt ]
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
              reset_at |> ResentAt.value |> Utils.Time.formatted_date_time)
            |> txt
          ; invitation.created_at
            |> CreatedAt.value
            |> Utils.Time.formatted_date_time
            |> txt
          ; form
              ~a:
                [ a_method `Post
                ; a_action
                    (form_action
                       ~path:
                         (Format.asprintf
                            "%s/resend"
                            (invitation.Invitation.id |> Id.value))
                       experiment.Experiment.id)
                ; a_class [ "flexrow"; "justify-end" ]
                ]
              [ Input.csrf_element csrf ()
              ; Input.submit_element
                  language
                  Message.(Resend (Some Field.Invitation))
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
end

let index
  experiment
  key_list
  template_list
  query_experiments
  query_tags
  filtered_contacts
  mailings
  ({ Pool_context.language; _ } as context)
  =
  let filter =
    let html =
      Partials.send_invitation
        context
        experiment
        key_list
        template_list
        query_experiments
        query_tags
        filtered_contacts
    in
    I18n.Filter, [ html ]
  in
  let mailings =
    let html =
      Page_admin_mailings.List.create
        true
        context
        experiment.Experiment.id
        mailings
    in
    I18n.Mailings, [ html ]
  in
  let sent_invitations =
    let html =
      p
        [ a
            ~a:
              [ a_href
                  (experiment.Experiment.id
                   |> Experiment.Id.value
                   |> Format.asprintf "admin/experiments/%s/invitations/sent"
                   |> Sihl.Web.externalize_path)
              ]
            [ txt (Utils.nav_link_to_string language I18n.SentInvitations) ]
        ]
    in
    I18n.SentInvitations, [ html ]
  in
  [ filter; mailings; sent_invitations ]
  |> Component.Collabsible.list language
  |> CCList.return
  |> Layout.Experiment.(
       create
         ~active_navigation:I18n.Invitations
         context
         (NavLink I18n.Invitations)
         experiment)
;;
