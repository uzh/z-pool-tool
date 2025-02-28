open Tyxml.Html
open Component.Input
open Pool_message
module DataTable = Component.DataTable

let form_action ?path id =
  let base =
    Format.asprintf "/admin/experiments/%s/invitations" (id |> Experiment.Id.value)
  in
  CCOption.map_or ~default:base (fun path -> Format.asprintf "%s/%s" base path) path
;;

module Partials = struct
  let list Pool_context.{ language; csrf; _ } experiment (invitations, query) =
    let open Invitation in
    let url = form_action ~path:"sent" experiment.Experiment.id |> Uri.of_string in
    let data_table =
      Component.DataTable.create_meta ~search:Invitation.searchable_by url query language
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
      let formatted_date = Pool_model.Time.formatted_date_time in
      let resend_form =
        let open Component.Input in
        form
          ~a:
            [ a_method `Post
            ; a_action
                (form_action
                   ~path:(Format.asprintf "%s/resend" (id |> Pool_common.Id.value))
                   experiment.Experiment.id
                 |> Sihl.Web.externalize_path)
            ; a_class [ "flexrow"; "justify-end" ]
            ]
          [ csrf_element csrf ()
          ; submit_element language Control.(Resend (Some Field.Invitation)) ()
          ]
      in
      let open CCFun in
      [ txt (Contact.lastname_firstname contact)
      ; txt (Contact.email_address contact |> Pool_user.EmailAddress.value)
      ; txt
          (resent_at
           |> CCOption.map_or
                ~default:""
                (ResentAt.value %> Pool_model.Time.formatted_date_time))
      ; txt (send_count |> SendCount.value |> CCInt.to_string)
      ; txt (created_at |> Pool_common.CreatedAt.value |> formatted_date)
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
      data_table
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
        statistics
    =
    let open Pool_common in
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
                 let id = Contact.id contact |> Contact.Id.value in
                 [ div
                     [ input
                         ~a:
                           [ a_input_type `Checkbox
                           ; a_name Field.(Contacts |> array_key)
                           ; a_id id
                           ; a_value id
                           ]
                         ()
                     ; label ~a:[ a_label_for id ] [ txt (Contact.fullname contact) ]
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
                    (form_action experiment.Experiment.id |> Sihl.Web.externalize_path)
                ; a_class [ "stack" ]
                ]
              [ csrf_element csrf ()
              ; rows
              ; submit_element
                  language
                  Control.(Send (Some Field.Invitation))
                  ~submit_type:`Success
                  ()
              ]
          ]
    in
    div
      [ h3
          ~a:[ a_class [ "heading-3" ] ]
          [ Control.(Filter (Some Field.Contacts))
            |> Utils.control_to_string language
            |> txt
          ]
      ; Unsafe.data (Utils.text_to_string language I18n.FilterContactsDescription)
        |> Component.Collapsible.create_note language
      ; Component.Filter.(
          filter_form
            ~statistics
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

let sent_invitations
      (Pool_context.{ language; _ } as context)
      experiment
      invitations
      statistics
  =
  let open Pool_common in
  let text_to_string = Utils.text_to_string language in
  div
    ~a:[ a_class [ "stack-lg" ] ]
    [ div
        ~a:[ a_class [ "grid-col-2" ] ]
        [ div
            ~a:[ a_class [ "stack-xs"; "inset"; "bg-grey-lighter"; "border" ] ]
            [ h3 [ txt (text_to_string I18n.InvitationsStatistics) ]
            ; p [ txt (text_to_string I18n.InvitationsStatisticsIntro) ]
            ; Component.Statistics.SentInvitations.create language statistics
            ]
        ]
    ; Partials.list context experiment invitations
    ]
  |> CCList.return
  |> Layout.Experiment.(
       create
         ~active_navigation:"invitations/sent"
         context
         (NavLink I18n.SentInvitations)
         experiment)
;;
