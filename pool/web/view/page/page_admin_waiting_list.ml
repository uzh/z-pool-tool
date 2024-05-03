open CCFun.Infix
open Tyxml.Html
open Pool_common
open Component.Input
open Pool_message
module Table = Component.Table
module DataTable = Component.DataTable

let list
  ?(access_contact_profiles = false)
  { Pool_context.language; _ }
  experiment
  (waiting_list_entries, query)
  =
  let open Pool_user in
  let url =
    experiment.Experiment.id
    |> Experiment.Id.value
    |> Format.asprintf "/admin/experiments/%s/waiting-list"
    |> Uri.of_string
  in
  let data_table =
    Component.DataTable.create_meta
      ~search:Waiting_list.searchable_by
      url
      query
      language
  in
  let cols =
    let open Pool_common in
    let to_string field =
      Utils.field_to_string language field |> CCString.capitalize_ascii |> txt
    in
    [ `column column_name
    ; `column column_email
    ; `custom (to_string Field.CellPhone)
    ; `column Waiting_list.column_signed_up_at
    ; `custom (to_string Field.AdminComment)
    ; `empty
    ]
  in
  let profile_link =
    Waiting_list.contact
    %> Contact.(id %> Id.value)
    %> Format.asprintf "/admin/contacts/%s"
    %> link_as_button
         ~is_text:true
         ~control:(language, Control.OpenProfile)
         ~icon:Component.Icon.PersonOutline
  in
  let th_class = [ "w-3"; "w-3"; "w-2"; "w-2"; "w-2" ] in
  let row
    ({ Waiting_list.contact; admin_comment; created_at; _ } as waiting_list :
      Waiting_list.t)
    =
    let edit =
      Waiting_list.(id %> Id.value)
      %> Format.asprintf "%s/%s" (Uri.to_string url)
      %> link_as_button
           ~is_text:true
           ~control:(language, Control.Edit None)
           ~icon:Component.Icon.CreateOutline
    in
    let buttons =
      [ true, edit; access_contact_profiles, profile_link ]
      |> CCList.filter_map (fun (active, form) ->
        if not active then None else Some (form waiting_list))
      |> Component.ButtonGroup.dropdown
      |> CCList.pure
    in
    [ Page_admin_contact.contact_lastname_firstname
        access_contact_profiles
        contact
    ; txt (Contact.email_address contact |> EmailAddress.value)
    ; txt
        (contact.Contact.cell_phone
         |> CCOption.map_or ~default:"" CellPhone.value)
    ; txt
        (created_at
         |> Pool_common.CreatedAt.value
         |> Pool_model.Time.formatted_date_time)
    ; admin_comment
      |> CCOption.map_or ~default:"" Waiting_list.AdminComment.value
      |> HttpUtils.first_n_characters
      |> HttpUtils.add_line_breaks
    ]
    @ buttons
    |> CCList.map CCFun.(CCList.return %> td)
    |> tr
  in
  DataTable.make
    ~target_id:"waiting-list"
    ~th_class
    ~cols
    ~row
    data_table
    waiting_list_entries
;;

let session_row language chronological session =
  let open Pool_common in
  let open Session in
  let is_followup = CCOption.is_some session.follow_up_to in
  let show_field field =
    Utils.field_to_string language field |> CCString.capitalize_ascii
  in
  let key_figures = Page_admin_session.Partials.session_key_figures session in
  let title =
    let date = span [ txt (session |> start_end_with_duration_human) ] in
    match is_followup, chronological with
    | false, true | false, false -> date
    | true, true ->
      div
        ~a:[ a_class [ "flexrow"; "flex-gap-sm" ] ]
        [ date; Page_admin_session.follow_up_icon language ]
    | true, false -> div ~a:[ a_class [ "inset"; "left" ] ] [ date ]
  in
  let waiting_list_radio_button language session =
    let open Pool_common in
    if Session.is_fully_booked session
    then span [ txt (Utils.error_to_string language Error.SessionFullyBooked) ]
    else if is_followup
    then
      span
        [ txt
            (Utils.error_to_string language Error.SessionRegistrationViaParent)
        ]
    else (
      match Session.assignment_creatable session |> CCResult.is_ok with
      | false -> txt ""
      | true ->
        input
          ~a:
            [ a_input_type `Radio
            ; a_name Field.(show Session)
            ; a_value Session.(session.id |> Id.value)
            ]
          ())
  in
  let cells =
    [ title, Some (show_field Field.Date)
    ; ( waiting_list_radio_button language session
      , Some Pool_common.(Utils.control_to_string language Control.Select) )
    ; ( txt (CCInt.to_string (session.assignment_count |> AssignmentCount.value))
      , Some (show_field Field.AssignmentCount) )
    ; txt key_figures, Some Page_admin_session.key_figures_head
    ]
  in
  let cell (value, label) =
    match label with
    | None -> td [ value ]
    | Some label -> td ~a:[ a_user_data "label" label ] [ value ]
  in
  cells |> CCList.map cell |> tr
;;

let session_list language chronological sessions =
  let thead =
    let to_txt = Table.field_to_txt language in
    [ to_txt Field.Date
    ; txt ""
    ; to_txt Field.AssignmentCount
    ; txt Page_admin_session.key_figures_head
    ]
    |> Component.Table.table_head
  in
  let rows =
    CCList.flat_map
      (fun (parent, follow_ups) ->
        let row = session_row language chronological in
        let parent = row parent in
        let follow_ups = CCList.map row follow_ups in
        parent :: follow_ups)
      sessions
  in
  let chronological_toggle =
    let open Page_admin_session in
    if sessions
       |> CCList.fold_left
            (fun acc (session, followups) -> acc @ (session :: followups))
            []
       |> some_session_is_followup
    then Page_admin_session.Partials.chronological_toggle language chronological
    else txt ""
  in
  div
    ~a:[ a_class [ "stack" ] ]
    [ chronological_toggle
    ; table
        ~a:
          [ a_class
              [ "table"
              ; "break-mobile"
              ; "session-list"
              ; "striped"
              ; "align-last-end"
              ]
          ]
        ~thead
        rows
    ]
;;

let detail
  (Waiting_list.{ id; contact; experiment; admin_comment; _ } : Waiting_list.t)
  sessions
  experiment_id
  (Pool_context.{ language; csrf; user; _ } as context)
  flash_fetcher
  chronological
  =
  let waiting_list_detail =
    div
      ~a:[ a_class [ "stack" ] ]
      [ Page_admin_contact.personal_detail user language contact
      ; form
          ~a:
            [ a_class [ "stack" ]
            ; a_method `Post
            ; a_action
                (Sihl.Web.externalize_path
                   (Format.asprintf
                      "/admin/experiments/%s/waiting-list/%s"
                      (Experiment.Id.value experiment.Experiment.id)
                      (Waiting_list.Id.value id)))
            ]
          [ csrf_element csrf ()
          ; textarea_element
              language
              Field.AdminComment
              ~value:
                (CCOption.map_or
                   ~default:""
                   Waiting_list.AdminComment.value
                   admin_comment)
              ~flash_fetcher
          ; div
              ~a:[ a_class [ "flexrow" ] ]
              [ submit_element
                  ~classnames:[ "push" ]
                  language
                  Control.(Save (Some Field.AdminComment))
                  ()
              ]
          ]
      ]
  in
  let sessions =
    let content =
      if CCList.is_empty sessions
      then
        div
          [ txt (Utils.text_to_string language (I18n.EmtpyList Field.Sessions))
          ]
      else
        session_list language chronological sessions
        |> fun content ->
        form
          ~a:
            [ a_method `Post
            ; a_action
                (Format.asprintf
                   "/admin/experiments/%s/waiting-list/%s/assign"
                   (experiment_id |> Experiment.Id.value)
                   (id |> Waiting_list.Id.value)
                 |> Sihl.Web.externalize_path)
            ]
          [ csrf_element csrf ()
          ; content
          ; div
              ~a:[ a_class [ "gap"; "flexrow" ] ]
              [ submit_element
                  ~classnames:[ "push" ]
                  language
                  (Control.Assign (Some Field.Contact))
                  ()
              ]
          ]
    in
    div
      [ h2
          ~a:[ a_class [ "heading-2" ] ]
          [ txt (Utils.nav_link_to_string language I18n.Sessions) ]
      ; p
          [ txt
              (I18n.AssignContactFromWaitingList
               |> Utils.hint_to_string language)
          ]
      ; content
      ]
  in
  div ~a:[ a_class [ "stack-lg" ] ] [ waiting_list_detail; sessions ]
  |> CCList.return
  |> Layout.Experiment.(create context (NavLink I18n.WaitingList) experiment)
;;

let index ?access_contact_profiles context experiment waiting_list =
  list ?access_contact_profiles context experiment waiting_list
  |> CCList.return
  |> Layout.Experiment.(
       create
         ~active_navigation:"waiting-list"
         ~hint:I18n.ExperimentWaitingList
         context
         (NavLink I18n.WaitingList)
         experiment)
;;
