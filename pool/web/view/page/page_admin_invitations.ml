open Tyxml.Html
open Component.Input
module DataTable = Component.DataTable

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
;;

module Partials = struct
  let list Pool_context.{ language; csrf; _ } experiment (invitations, query) =
    let open Invitation in
    let url =
      form_action ~path:"sent" experiment.Experiment.id |> Uri.of_string
    in
    let datatable =
      DataTable.{ url; query; language; search = Some Invitation.searchable_by }
    in
    let cols =
      [ `column Pool_user.column_name
      ; `column Pool_user.column_email
      ; `column column_resent_at
      ; `column column_count
      ; `column column_created_at
      ; `empty
      ]
    in
    let th_class = [ "w-2"; "w-3"; "w-2"; "w-1"; "w-2"; "w-2" ] in
    let row ({ id; contact; resent_at; send_count; created_at; _ } : t) =
      let formatted_date = Pool_common.Utils.Time.formatted_date_time in
      let resend_form =
        let open Component.Input in
        form
          ~a:
            [ a_method `Post
            ; a_action
                (form_action
                   ~path:
                     (Format.asprintf "%s/resend" (id |> Pool_common.Id.value))
                   experiment.Experiment.id
                 |> Sihl.Web.externalize_path)
            ; a_class [ "flexrow"; "justify-end" ]
            ]
          [ csrf_element csrf ()
          ; submit_element
              language
              Pool_common.Message.(Resend (Some Field.Invitation))
              ()
          ]
      in
      let open CCFun in
      [ txt (Contact.lastname_firstname contact)
      ; txt (Contact.email_address contact |> Pool_user.EmailAddress.value)
      ; txt
          (resent_at
           |> CCOption.map_or
                ~default:""
                (ResentAt.value %> Pool_common.Utils.Time.formatted_date_time))
      ; txt (send_count |> SendCount.value |> CCInt.to_string)
      ; txt (formatted_date created_at)
      ; resend_form
      ]
      |> CCList.map (CCList.return %> td)
      |> tr
    in
    DataTable.make
      ~th_class
      ~target_id:"experiment-list"
      ~cols
      ~row
      datatable
      invitations
  ;;

  let send_invitation
    Pool_context.{ csrf; language; _ }
    experiment
    key_list
    template_list
    query_experiments
    query_tags
    filtered_contacts
    matching_filter_count
    invitation_count
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
            |> Component.Table.horizontal_table `Striped
        in
        div
          [ h4
              ~a:[ a_class [ "heading-4" ] ]
              [ txt "Filtered contacts (development only)" ]
          ; form
              ~a:
                [ a_method `Post
                ; a_action
                    (form_action experiment.Experiment.id
                     |> Sihl.Web.externalize_path)
                ; a_class [ "stack" ]
                ]
              [ csrf_element csrf ()
              ; rows
              ; submit_element
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
      ; Unsafe.data
          Pool_common.(
            Utils.text_to_string language I18n.FilterContactsDescription)
        |> Component.Collapsible.create_note language
      ; Component.Filter.(
          filter_form
            ~counts:(matching_filter_count, invitation_count)
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
                Utils.text_to_string language I18n.InvitationsStatistics)
          ]
      ; p
          [ txt
              Pool_common.(
                Utils.text_to_string language I18n.InvitationsStatisticsIntro)
          ]
      ; (sent_by_count
         |> CCList.map (fun (key, value) ->
           (to_string key, to_string value) |> to_row)
         |> fun rows ->
         rows @ [ total ] |> table ~thead ~a:[ a_class [ "table"; "simple" ] ])
      ]
  ;;
end

let sent_invitations
  (Pool_context.{ language; _ } as context)
  experiment
  invitations
  statistics
  =
  let open Pool_common in
  div
    ~a:[ a_class [ "stack-lg" ] ]
    [ div
        ~a:[ a_class [ "grid-col-2" ] ]
        [ div
            ~a:[ a_class [ "stack-xs"; "inset"; "bg-grey-light"; "border" ] ]
            [ Partials.statistics language statistics ]
        ]
    ; Partials.list context experiment invitations
    ]
  |> CCList.return
  |> Layout.Experiment.(
       create
         ~active_navigation:I18n.Invitations
         context
         (I18n I18n.SentInvitations)
         experiment)
;;
