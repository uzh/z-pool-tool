open CCFun.Infix
open Tyxml.Html
open Component.Input
module Field = Pool_common.Message.Field
module Status = Component.UserStatus.Contact

let assignments_path experiment_id =
  Format.asprintf
    "/admin/experiments/%s/assignments"
    (experiment_id |> Experiment.Id.value)
;;

let assignment_specific_path ?suffix experiment_id session_id assignment_id =
  let open Pool_common in
  let base =
    Format.asprintf
      "/admin/experiments/%s/sessions/%s/%s/%s"
      (experiment_id |> Experiment.Id.value)
      Session.(session_id |> Id.value)
      Message.Field.(human_url Assignments)
      Assignment.(assignment_id |> Id.value)
  in
  suffix |> CCOption.map_or ~default:base (Format.asprintf "%s/%s" base)
;;

let swap_session_modal_id session =
  Format.asprintf "swap-session-%s" Session.(Id.value session.Session.id)
;;

type assignment_redirect =
  | Assignments
  | DeletedAssignments
  | Session
[@@deriving show { with_path = false }, yojson]

let read_assignment_redirect m =
  try Some (Utils.Json.read_variant assignment_redirect_of_yojson m) with
  | _ -> None
;;

module Partials = struct
  open Assignment

  let empty language =
    Pool_common.(
      Utils.text_to_string language (I18n.EmtpyList Message.Field.Assignments))
    |> txt
  ;;

  (* TODO[timhub]: replace with icon when it is added to framework *)
  let boolean_value = function
    | false -> "x" |> txt
    | true -> "✓" |> txt
  ;;

  let assignment_id { Assignment.id; _ } = id |> Assignment.Id.value |> txt

  let assignment_participated { Assignment.participated; _ } =
    participated
    |> CCOption.map_or ~default:(txt "") (Participated.value %> boolean_value)
  ;;

  let assignment_no_show { Assignment.no_show; _ } =
    no_show |> CCOption.map_or ~default:(txt "") (NoShow.value %> boolean_value)
  ;;

  let assignment_external_data_id { Assignment.external_data_id; _ } =
    external_data_id |> CCOption.map_or ~default:"" ExternalDataId.value |> txt
  ;;

  let contact_firstname ({ Assignment.contact; _ } : Assignment.t) =
    contact |> Contact.firstname |> Pool_user.Firstname.value |> txt
  ;;

  let contact_lastname ({ Assignment.contact; _ } : Assignment.t) =
    contact |> Contact.lastname |> Pool_user.Lastname.value |> txt
  ;;

  let contact_lastname_firstname ({ Assignment.contact; _ } : Assignment.t) =
    contact |> Contact.lastname_firstname |> txt
  ;;

  let contact_email ({ Assignment.contact; _ } : Assignment.t) =
    contact |> Contact.email_address |> Pool_user.EmailAddress.value |> txt
  ;;

  let contact_cellphone ({ Assignment.contact; _ } : Assignment.t) =
    contact
    |> fun { Contact.cell_phone; _ } ->
    CCOption.map_or ~default:"" Pool_user.CellPhone.value cell_phone |> txt
  ;;

  let canceled_at ({ Assignment.canceled_at; _ } : Assignment.t) =
    canceled_at
    |> CCOption.map_or
         ~default:""
         (Assignment.CanceledAt.value
          %> Pool_common.Utils.Time.formatted_date_time)
    |> txt
  ;;

  module ReminderModal = struct
    let modal_id id = Format.asprintf "%s-reminder" (Assignment.Id.value id)
    let control = Pool_common.Message.(Send (Some Field.Reminder))
    let title language = Pool_common.(Utils.control_to_string language control)

    let modal
      { Pool_context.language; csrf; _ }
      experiment_id
      session
      { Assignment.id; contact; reminder_manually_last_sent_at; _ }
      =
      let open Pool_common.Reminder in
      let action =
        assignment_specific_path
          ~suffix:"remind"
          experiment_id
          session.Session.id
          id
        |> Sihl.Web.externalize_path
      in
      let available_channels =
        let open Channel in
        match contact.Contact.cell_phone with
        | None -> CCList.remove ~eq:equal ~key:TextMessage all
        | Some _ -> all
      in
      let html =
        let format = Component.Utils.format_reminder_sent_opt ~default:"-" in
        let timestamps =
          let open Session in
          let to_list_item ?(classnames = []) (label, value) =
            li
              ~a:[ a_class classnames ]
              [ txt
                  (Pool_common.Utils.field_to_string language label
                   |> CCString.capitalize_ascii)
              ; txt ": "
              ; format value
              ]
          in
          let session_timestamps =
            [ Field.EmailRemindersSentAt, session.email_reminder_sent_at
            ; ( Field.TextMessageRemindersSentAt
              , session.text_message_reminder_sent_at )
            ]
            |> CCList.map to_list_item
          in
          let assignment_timestamps =
            (Field.LastManuallyRemindedAt, reminder_manually_last_sent_at)
            |> to_list_item ~classnames:[ "border-top"; "inset-xs"; "top" ]
          in
          session_timestamps @ [ assignment_timestamps ]
          |> ul ~a:[ a_class [ "no-style" ] ]
        in
        div
          [ timestamps
          ; form
              ~a:[ a_method `Post; a_action action; a_class [ "stack" ] ]
              [ csrf_element csrf ()
              ; selector
                  language
                  Field.MessageChannel
                  Channel.show
                  available_channels
                  None
                  ~option_formatter:(fun channel ->
                    Channel.show channel
                    |> CCString.replace ~sub:"_" ~by:" "
                    |> CCString.capitalize_ascii)
                  ()
              ; submit_element language Pool_common.Message.(Send None) ()
              ]
          ]
      in
      Component.Modal.create language title (modal_id id) html
    ;;

    let button { Pool_context.language; _ } { Assignment.id; _ } =
      a
        ~a:
          [ a_href "#"
          ; a_user_data "modal" (modal_id id)
          ; a_class [ "has-icon"; "primary"; "btn"; "is-text" ]
          ]
        [ Component.Icon.(to_html MailOutline); txt (title language) ]
    ;;
  end

  let swap_session_form
    ({ Pool_context.language; csrf; _ } as context)
    experiment
    session
    assignment
    assigned_sessions
    available_sessions
    swap_session_template
    languages
    flash_fetcher
    =
    let action =
      assignment_specific_path
        ~suffix:"swap-session"
        experiment.Experiment.id
        session.Session.id
        assignment.Assignment.id
      |> Sihl.Web.externalize_path
    in
    let option_formatter m =
      let open Session in
      let str = start_end_to_human m in
      match m.follow_up_to with
      | Some _ -> Unsafe.data (Format.asprintf "&nbsp;&nbsp;&nbsp;%s" str)
      | None -> txt str
    in
    let option_disabler m =
      Session.can_be_assigned_to_existing_assignment m |> CCResult.is_error
      || CCList.mem ~eq:Session.equal m assigned_sessions
    in
    let notifier_id =
      Format.asprintf "nofify-%s" Session.(Id.value session.id)
    in
    let html =
      match available_sessions with
      | [] ->
        p
          [ txt
              Pool_common.(
                Utils.text_to_string language I18n.SwapSessionsListEmpty)
          ]
      | available_sessions ->
        div
          ~a:[ a_class [ "stack" ] ]
          [ Component.Notification.notification
              language
              `Warning
              [ Pool_common.(Utils.hint_to_string language I18n.SwapSessions)
                |> HttpUtils.add_line_breaks
              ]
          ; form
              ~a:[ a_method `Post; a_class [ "stack" ]; a_action action ]
              [ selector
                  ~required:true
                  ~add_empty:true
                  ~elt_option_formatter:option_formatter
                  ~option_disabler
                  language
                  Field.Session
                  (fun s -> s.Session.id |> Session.Id.value)
                  available_sessions
                  None
                  ()
              ; checkbox_element
                  ~additional_attributes:[ a_user_data "toggle" notifier_id ]
                  language
                  Field.NotifyContact
              ; div
                  ~a:[ a_id notifier_id; a_class [ "hidden"; "stack" ] ]
                  (Page_admin_message_template.template_inputs
                     ~hide_text_message_input:true
                     context
                     (Some swap_session_template)
                     ~languages
                     ?fixed_language:experiment.Experiment.language
                     ~selected_language:
                       swap_session_template.Message_template.language
                     flash_fetcher)
              ; submit_element
                  language
                  (Pool_common.Message.Save None)
                  ~submit_type:`Primary
                  ()
              ; csrf_element csrf ()
              ]
          ]
    in
    Component.Modal.create
      ~active:true
      language
      (fun lang ->
        Pool_common.(Utils.control_to_string lang Message.ChangeSession))
      (swap_session_modal_id session)
      html
  ;;

  let overview_list
    ?(access_contact_profiles = false)
    ?(view_contact_name = false)
    ?(view_contact_info = false)
    ?(allow_session_swap = false)
    ?(is_print = false)
    redirect
    (Pool_context.{ language; csrf; _ } as context)
    experiment
    session
    assignments
    =
    let open Pool_common in
    let default = txt "" in
    let assignemnts_table_id =
      Format.asprintf "assignments-%s" Session.(Id.value session.Session.id)
    in
    let swap_session_modal_id = swap_session_modal_id session in
    let deletable = CCFun.(Assignment.is_deletable %> CCResult.is_ok) in
    let cancelable m =
      Session.assignments_cancelable session |> CCResult.is_ok
      && Assignment.is_cancellable m |> CCResult.is_ok
    in
    let session_changeable m =
      Assignment.session_changeable session m |> CCResult.is_ok
      && allow_session_swap
    in
    let action { Assignment.id; _ } suffix =
      assignment_specific_path
        ~suffix
        experiment.Experiment.id
        session.Session.id
        id
    in
    let create_reminder_modal assignment =
      Assignment.reminder_sendable session assignment |> CCResult.is_ok
    in
    let button_form
      ?(style = `Primary)
      suffix
      confirmable
      control
      icon
      assignment
      =
      let hidden_redirect_input =
        input_element
          ~value:(show_assignment_redirect redirect)
          language
          `Hidden
          Field.Redirect
      in
      form
        ~a:
          [ a_action (action assignment suffix |> Sihl.Web.externalize_path)
          ; a_method `Post
          ; a_user_data
              "confirmable"
              Pool_common.(Utils.confirmable_to_string language confirmable)
          ]
        [ csrf_element csrf ()
        ; hidden_redirect_input
        ; submit_element
            ~is_text:true
            ~submit_type:style
            ~has_icon:icon
            language
            control
            ()
        ]
    in
    let edit m =
      let action = action m "edit" in
      link_as_button
        action
        ~is_text:true
        ~control:(language, Pool_common.Message.(Edit None))
        ~icon:Component.Icon.CreateOutline
    in
    let profile_link { Assignment.contact; _ } =
      let action =
        Format.asprintf "/admin/contacts/%s" Contact.(id contact |> Id.value)
      in
      link_as_button
        action
        ~is_text:true
        ~control:(language, Pool_common.Message.OpenProfile)
        ~icon:Component.Icon.PersonOutline
    in
    let external_data_ids { Assignment.contact; _ } =
      a
        ~a:
          [ a_href
              (Format.asprintf
                 "%s/%s"
                 (Page_admin_contact.path contact)
                 Field.(human_url ExternalDataId)
               |> Sihl.Web.externalize_path)
          ; a_class [ "has-icon"; "primary"; "btn"; "is-text" ]
          ]
        [ Icon.(to_html ReorderThree)
        ; txt Utils.(nav_link_to_string language I18n.ExternalDataIds)
        ]
    in
    let session_change_toggle { Assignment.id; _ } =
      let action =
        assignment_specific_path
          ~suffix:"swap-session"
          experiment.Experiment.id
          session.Session.id
          id
        |> Sihl.Web.externalize_path
      in
      link_as_button
        "#"
        ~attributes:
          [ a_user_data "hx-trigger" "click"
          ; a_user_data "hx-get" action
          ; a_user_data "hx-swap" "outerHTML"
          ; a_user_data
              "hx-target"
              (Format.asprintf "#%s" swap_session_modal_id)
          ]
        ~is_text:true
        ~control:(language, Pool_common.Message.ChangeSession)
        ~icon:Component.Icon.SwapHorizonal
    in
    let cancel =
      button_form
        ~style:`Error
        "cancel"
        I18n.(
          if session.Session.has_follow_ups
          then CancelAssignmentWithFollowUps
          else CancelAssignment)
        (Message.Cancel None)
        Component.Icon.Close
    in
    let mark_as_deleted =
      button_form
        ~style:`Error
        "mark-as-deleted"
        I18n.(
          if session.Session.has_follow_ups
          then MarkAssignmentWithFollowUpsAsDeleted
          else MarkAssignmentAsDeleted)
        Message.MarkAsDeleted
        Component.Icon.TrashOutline
    in
    match CCList.is_empty assignments with
    | true -> p [ empty language ]
    | false ->
      let add_field_if check values = if check then values else [] in
      let contact_information =
        add_field_if
          view_contact_name
          [ Field.Lastname, contact_lastname
          ; Field.Firstname, contact_firstname
          ]
        @ add_field_if view_contact_info [ Field.Email, contact_email ]
        @ add_field_if view_contact_info [ Field.CellPhone, contact_cellphone ]
        |> function
        | [] -> [ Field.Id, assignment_id ]
        | fields -> fields
      in
      let external_data_field =
        add_field_if
          Experiment.(external_data_required_value experiment)
          [ Field.ExternalDataId, assignment_external_data_id ]
      in
      let thead =
        let field_to_text = Component.Table.field_to_txt language in
        let left =
          CCList.map (fun (field, _) -> field_to_text field) contact_information
          @ CCList.map
              (fun (field, _) -> field_to_text field)
              external_data_field
        in
        let checkboxes = [ txt "P"; txt "NS" ] in
        let right =
          let base = [ Field.CanceledAt |> field_to_text ] in
          if is_print then base else base @ [ default ]
        in
        left @ checkboxes @ right
      in
      let rows, modals =
        CCList.fold_left
          (fun (rows, modals) (assignment : Assignment.t) ->
            let base =
              CCList.map snd contact_information
              @ CCList.map snd external_data_field
              @ [ assignment_participated; assignment_no_show; canceled_at ]
              |> CCList.mapi (fun i fcn ->
                let value = fcn assignment in
                if CCInt.equal i 0
                then
                  div
                    ~a:[ a_class [ "flexrow"; "flex-gap-sm" ] ]
                    (value :: Status.make_icons assignment.contact `Name)
                else value)
            in
            let buttons =
              [ true, edit
              ; access_contact_profiles, profile_link
              ; create_reminder_modal assignment, ReminderModal.button context
              ; ( Experiment.(show_external_data_id_links_value experiment)
                , external_data_ids )
              ; session_changeable assignment, session_change_toggle
              ; cancelable assignment, cancel
              ; deletable assignment, mark_as_deleted
              ]
              |> CCList.filter_map (fun (active, form) ->
                if not active then None else Some (form assignment))
              |> Component.ButtonGroup.dropdown
              |> CCList.pure
            in
            let modals =
              match create_reminder_modal assignment with
              | true ->
                modals
                @ [ ReminderModal.modal
                      context
                      experiment.Experiment.id
                      session
                      assignment
                  ]
              | false -> modals
            in
            let columns = if is_print then base else base @ buttons in
            rows @ [ columns ], modals)
          ([], [])
          assignments
      in
      let js =
        Format.asprintf
          {js|
          %s

          document.addEventListener("htmx:afterSwap", (e) => {
            const modal = e.detail.elt;
            window['pool-tool'].initRichTextEditor(modal);

            modal.querySelector(".modal-close").addEventListener("click", (e) => {
              modal.classList.remove("active");
              modal.setAttribute("aria-hidden", "true");
            })

            const checkbox = modal.querySelector(`[data-toggle]`);
            const target = document.getElementById(checkbox.dataset.toggle);
            checkbox.addEventListener("click", (e) => {
              if(e.currentTarget.checked) {
                target.classList.remove("hidden");
              } else {
                target.classList.add("hidden");
              }
            })
          })
          |js}
          (Component.Modal.js_modal_add_spinner swap_session_modal_id)
      in
      div
        [ p
            [ Utils.hint_to_string language I18n.SessionCloseLegend
              |> HttpUtils.add_line_breaks
            ]
        ; div
            ~a:
              [ a_id swap_session_modal_id
              ; a_class [ "fullscreen-overlay"; "modal" ]
              ]
            []
        ; Component.Table.horizontal_table
            `Striped
            ~align_last_end:true
            ~id:assignemnts_table_id
            ~thead
            rows
        ; div ~a:[ a_class [ "assignment-reminder-modals" ] ] modals
        ; (if allow_session_swap then script (Unsafe.data js) else txt "")
        ]
  ;;

  let grouped_overview_lists
    ?access_contact_profiles
    ?view_contact_name
    ?view_contact_info
    redirect
    (Pool_context.{ language; _ } as context)
    experiment
    assignments
    =
    CCList.map
      (fun (session, assignments) ->
        let attrs, to_title =
          if CCOption.is_some session.Session.follow_up_to
          then
            ( [ a_class [ "inset"; "left" ] ]
            , fun session ->
                Format.asprintf
                  "%s (%s)"
                  (session |> Session.session_date_to_human)
                  (Pool_common.Utils.field_to_string
                     language
                     Field.FollowUpSession) )
          else [], Session.session_date_to_human
        in
        div
          ~a:attrs
          [ h3 ~a:[ a_class [ "heading-3" ] ] [ txt (session |> to_title) ]
          ; overview_list
              ?access_contact_profiles
              ?view_contact_name
              ?view_contact_info
              redirect
              context
              experiment
              session
              assignments
          ])
      assignments
    |> div ~a:[ a_class [ "stack-lg" ] ]
  ;;
end

let data_table
  ?(access_contact_profiles = false)
  ?(view_contact_name = false)
  ?(view_contact_info = false)
  ?(is_print = false)
  redirect
  (Pool_context.{ language; csrf; _ } as context)
  experiment
  session
  (assignments, query)
  =
  let open Pool_common in
  let open Partials in
  let open Assignment in
  let url =
    Format.asprintf
      "/admin/experiments/%s/sessions/%s"
      (experiment.Experiment.id |> Experiment.Id.value)
      (session.Session.id |> Session.Id.value)
    |> Uri.of_string
  in
  let data_table =
    Component.DataTable.
      { url; query; language; search = Some Assignment.searchable_by }
  in
  let conditional_left_columns =
    [ view_contact_name, Pool_user.column_name, contact_lastname_firstname
    ; view_contact_info, Pool_user.column_email, contact_email
    ; view_contact_info, Contact.column_cell_phone, contact_cellphone
    ]
  in
  let conditional_right_columns =
    [ ( Experiment.(external_data_required_value experiment)
      , column_external_data_id
      , assignment_external_data_id )
    ; true, column_canceled_at, canceled_at
    ]
  in
  let swap_session_modal_id = swap_session_modal_id session in
  let deletable = CCFun.(Assignment.is_deletable %> CCResult.is_ok) in
  let cancelable m =
    Session.assignments_cancelable session |> CCResult.is_ok
    && Assignment.is_cancellable m |> CCResult.is_ok
  in
  let session_changeable m =
    Assignment.session_changeable session m |> CCResult.is_ok
  in
  let action { Assignment.id; _ } suffix =
    assignment_specific_path
      ~suffix
      experiment.Experiment.id
      session.Session.id
      id
  in
  let create_reminder_modal assignment =
    Assignment.reminder_sendable session assignment |> CCResult.is_ok
  in
  let button_form ?(style = `Primary) suffix confirmable control icon assignment
    =
    let hidden_redirect_input =
      input_element
        ~value:(show_assignment_redirect redirect)
        language
        `Hidden
        Field.Redirect
    in
    form
      ~a:
        [ a_action (action assignment suffix |> Sihl.Web.externalize_path)
        ; a_method `Post
        ; a_user_data
            "confirmable"
            Pool_common.(Utils.confirmable_to_string language confirmable)
        ]
      [ csrf_element csrf ()
      ; hidden_redirect_input
      ; submit_element
          ~is_text:true
          ~submit_type:style
          ~has_icon:icon
          language
          control
          ()
      ]
  in
  let edit m =
    let action = action m "edit" in
    link_as_button
      action
      ~is_text:true
      ~control:(language, Pool_common.Message.(Edit None))
      ~icon:Component.Icon.CreateOutline
  in
  let profile_link { Assignment.contact; _ } =
    let action =
      Format.asprintf "/admin/contacts/%s" Contact.(id contact |> Id.value)
    in
    link_as_button
      action
      ~is_text:true
      ~control:(language, Pool_common.Message.OpenProfile)
      ~icon:Component.Icon.PersonOutline
  in
  let external_data_ids { Assignment.contact; _ } =
    a
      ~a:
        [ a_href
            (Format.asprintf
               "%s/%s"
               (Page_admin_contact.path contact)
               Field.(human_url ExternalDataId)
             |> Sihl.Web.externalize_path)
        ; a_class [ "has-icon"; "primary"; "btn"; "is-text" ]
        ]
      [ Icon.(to_html ReorderThree)
      ; txt Utils.(nav_link_to_string language I18n.ExternalDataIds)
      ]
  in
  let session_change_toggle { Assignment.id; _ } =
    let action =
      assignment_specific_path
        ~suffix:"swap-session"
        experiment.Experiment.id
        session.Session.id
        id
      |> Sihl.Web.externalize_path
    in
    link_as_button
      "#"
      ~attributes:
        [ a_user_data "hx-trigger" "click"
        ; a_user_data "hx-get" action
        ; a_user_data "hx-swap" "outerHTML"
        ; a_user_data "hx-target" (Format.asprintf "#%s" swap_session_modal_id)
        ]
      ~is_text:true
      ~control:(language, Pool_common.Message.ChangeSession)
      ~icon:Component.Icon.SwapHorizonal
  in
  let cancel =
    button_form
      ~style:`Error
      "cancel"
      I18n.(
        if session.Session.has_follow_ups
        then CancelAssignmentWithFollowUps
        else CancelAssignment)
      (Message.Cancel None)
      Component.Icon.Close
  in
  let mark_as_deleted =
    button_form
      ~style:`Error
      "mark-as-deleted"
      I18n.(
        if session.Session.has_follow_ups
        then MarkAssignmentWithFollowUpsAsDeleted
        else MarkAssignmentAsDeleted)
      Message.MarkAsDeleted
      Component.Icon.TrashOutline
  in
  match CCList.is_empty assignments with
  | true -> p [ empty language ]
  | false ->
    let cols =
      let left =
        conditional_left_columns
        |> CCList.filter_map (fun (check, column, _) ->
          if check then Some (`column column) else None)
      in
      let center = [ `column column_participated; `column column_no_show ] in
      let right =
        conditional_right_columns
        |> CCList.filter_map (fun (check, column, _) ->
          if check then Some (`column column) else None)
      in
      let base = left @ center @ right in
      if is_print then base else base @ [ `empty ]
    in
    let row (assignment : t) =
      let left =
        conditional_left_columns
        |> CCList.filter_map (fun (check, _, to_html) ->
          if check then Some (to_html assignment) else None)
      in
      let center =
        [ assignment_participated assignment; assignment_no_show assignment ]
      in
      let right =
        conditional_right_columns
        |> CCList.filter_map (fun (check, _, to_html) ->
          if check then Some (to_html assignment) else None)
      in
      let buttons =
        [ true, edit
        ; access_contact_profiles, profile_link
        ; create_reminder_modal assignment, ReminderModal.button context
        ; ( Experiment.(show_external_data_id_links_value experiment)
          , external_data_ids )
        ; session_changeable assignment, session_change_toggle
        ; cancelable assignment, cancel
        ; deletable assignment, mark_as_deleted
        ]
        |> CCList.filter_map (fun (active, form) ->
          if not active then None else Some (form assignment))
        |> Component.ButtonGroup.dropdown
        |> CCList.pure
      in
      let base = left @ center @ right in
      (if is_print then base else base @ buttons)
      |> CCList.map (CCList.return %> td)
      |> tr
    in
    let modals =
      CCList.filter_map
        (fun assignment ->
          match create_reminder_modal assignment with
          | true ->
            Some
              (ReminderModal.modal
                 context
                 experiment.Experiment.id
                 session
                 assignment)
          | false -> None)
        assignments
      |> function
      | [] -> None
      | modals ->
        Some (div ~a:[ a_class [ "assignment-reminder-modals" ] ] modals)
    in
    Component.DataTable.make
      ~target_id:"assignments-table"
      ~cols
      ~row
      ?prepend_html:modals
      data_table
      assignments
;;

let list
  ?access_contact_profiles
  ?view_contact_name
  ?view_contact_info
  ({ Experiment.id; _ } as experiment)
  ({ Pool_context.language; _ } as context)
  assignments
  =
  [ div
      [ p
          [ a
              ~a:
                [ a_href
                    (assignments_path id
                     |> Format.asprintf "%s/deleted"
                     |> Sihl.Web.externalize_path)
                ]
              [ txt
                  Pool_common.(
                    Utils.text_to_string language I18n.DeletedAssignments)
              ]
          ]
      ; Partials.grouped_overview_lists
          ?access_contact_profiles
          ?view_contact_name
          ?view_contact_info
          Assignments
          context
          experiment
          assignments
      ]
  ]
  |> Layout.Experiment.(
       create
         ~active_navigation:Pool_common.I18n.Assignments
         ~hint:Pool_common.I18n.ExperimentAssignment
         context
         (NavLink Pool_common.I18n.Assignments)
         experiment)
;;

let marked_as_deleted
  ?access_contact_profiles
  ?view_contact_name
  ?view_contact_info
  experiment
  (Pool_context.{ language; _ } as context)
  assignments
  =
  let html =
    let notification =
      let open Pool_common in
      [ I18n.AssignmentsMarkedAsClosed |> Utils.hint_to_string language |> txt ]
      |> div
      |> CCList.pure
      |> Component.Notification.notification language `Warning
    in
    let list =
      Partials.grouped_overview_lists
        ?access_contact_profiles
        ?view_contact_name
        ?view_contact_info
        DeletedAssignments
        context
        experiment
        assignments
    in
    div ~a:[ a_class [ "stack-lg" ] ] [ notification; list ] |> CCList.return
  in
  Layout.Experiment.(
    create
      ~active_navigation:Pool_common.I18n.Assignments
      ~hint:Pool_common.I18n.ExperimentAssignment
      context
      (I18n Pool_common.I18n.DeletedAssignments)
      experiment
      html)
;;

let edit
  ({ Pool_context.language; csrf; _ } as context)
  view_contact_name
  experiment
  session
  { Assignment.id; no_show; participated; external_data_id; contact; _ }
  =
  let open Assignment in
  let open Component.Input in
  let open CCOption.Infix in
  let action =
    assignment_specific_path experiment.Experiment.id session.Session.id id
    |> Sihl.Web.externalize_path
  in
  let session_data =
    let open Session in
    div
      ~a:[ a_class [ "stack"; "inset"; "border"; " bg-grey-light" ] ]
      [ h3
          [ txt
              Pool_common.(
                Utils.field_to_string language Message.Field.Session
                |> CCString.capitalize_ascii)
          ]
      ; p [ session |> Session.start_end_to_human |> txt ]
      ; Component.Location.preview session.location
      ]
  in
  [ div
      ~a:[ a_class [ "switcher"; "flex-gap" ] ]
      [ div
          ~a:[ a_class [ "stack" ] ]
          [ Component.Notification.notification
              language
              `Warning
              [ txt
                  Pool_common.(
                    Utils.text_to_string language I18n.AssignmentEditTagsWarning)
              ]
          ; p
              [ Unsafe.data
                  Pool_common.(
                    Utils.hint_to_string language I18n.SessionCloseHints)
              ]
          ; form
              ~a:[ a_action action; a_method `Post ]
              [ csrf_element csrf ()
              ; div
                  ~a:[ a_class [ "flexcolumn"; "stack" ] ]
                  [ checkbox_element
                      ?value:(no_show >|= NoShow.value)
                      language
                      Field.NoShow
                  ; checkbox_element
                      ?value:(participated >|= Participated.value)
                      language
                      Field.Participated
                  ; input_element
                      ?value:(external_data_id >|= ExternalDataId.value)
                      language
                      `Text
                      Field.ExternalDataId
                  ; submit_element
                      language
                      ~classnames:[ "align-self-end" ]
                      Pool_common.Message.(Save None)
                      ()
                  ]
              ]
          ]
      ; session_data
      ]
  ]
  |> Layout.Experiment.(
       create
         context
         (Text
            (Status.identity
               view_contact_name
               contact
               (Assignment.Id.to_common id)))
         experiment)
;;
