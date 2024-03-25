open CCFun.Infix
open Tyxml.Html
open Component.Input
module Field = Pool_common.Message.Field
module Status = Component.UserStatus.Contact

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

let direct_message_modal_id = Format.asprintf "direct-message-modal"

module Partials = struct
  open Assignment

  let table_legend ?(hide_deleted = false) language =
    let open Pool_common in
    let open Component.Table in
    let hint_to_string = Utils.hint_to_string language in
    let field_to_string = Utils.field_to_string language in
    let deleted =
      [ ( field_to_string Field.MarkedAsDeleted
        , legend_color_item "bg-red-lighter" )
      ]
    in
    let base =
      I18n.
        [ hint_to_string SessionCloseLegendNoShow, legend_text_item "NS"
        ; hint_to_string SessionCloseLegendParticipated, legend_text_item "P"
        ; ( field_to_string Field.ExternalDataId
          , legend_text_item Field.(ExternalDataIdAbbr |> show) )
        ; field_to_string Field.Canceled, legend_color_item "bg-orange-lighter"
        ]
    in
    (if hide_deleted then base else base @ deleted) |> table_legend
  ;;

  let empty language =
    Pool_common.(
      Utils.text_to_string language (I18n.EmtpyList Message.Field.Assignments))
    |> txt
  ;;

  let assignment_id { Assignment.id; _ } = id |> Assignment.Id.value |> txt

  let assignment_participated { Assignment.participated; _ } =
    participated
    |> CCOption.map_or
         ~default:(txt "")
         (Participated.value %> Icon.bool_to_icon)
  ;;

  let assignment_no_show { Assignment.no_show; _ } =
    no_show
    |> CCOption.map_or ~default:(txt "") (NoShow.value %> Icon.bool_to_icon)
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

  let custom_fields_header language custom_fields =
    CCList.map
      (fun field -> div [ txt (Custom_field.name_value language field) ])
      custom_fields
    |> div ~a:[ a_class [ "flexcolumn" ] ]
  ;;

  let custom_field_cells language user answers custom_fields =
    let open Custom_field in
    answers
    |> CCOption.map_or ~default:[] (fun custom_data ->
      CCList.map
        (fun field ->
          CCList.find_opt
            (fun public -> public |> Public.id |> Id.equal (id field))
            custom_data
          |> CCOption.map_or
               ~default:(div [ txt "" ])
               (Component.CustomField.answer_to_html
                  ~add_data_label:true
                  user
                  language))
        custom_fields)
    |> div ~a:[ a_class [ "flexcolumn" ] ]
  ;;

  module ReminderModal = struct
    let modal_id id = Format.asprintf "reminder-%s" (Assignment.Id.value id)
    let control = Pool_common.Message.(Send (Some Field.Reminder))
    let title language = Pool_common.(Utils.control_to_string language control)

    let modal
      { Pool_context.language; csrf; _ }
      experiment_id
      session
      { Assignment.id; contact; reminder_manually_last_sent_at; _ }
      text_messages_enabled
      =
      let open Pool_common in
      let action =
        assignment_specific_path
          ~suffix:"remind"
          experiment_id
          session.Session.id
          id
        |> Sihl.Web.externalize_path
      in
      let available_channels =
        MessageChannel.filtered_channels
          (CCOption.is_some contact.Contact.cell_phone && text_messages_enabled)
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
              ; Component.Input.message_channel_select
                  language
                  available_channels
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

  let swap_session_notification_form_fields
    context
    (experiment : Experiment.t)
    session_id
    assignment_id
    languages
    swap_session_template
    flash_fetcher
    text_messages_disabled
    =
    let id = "swap-session-notification-form" in
    let language_select_attriutes =
      if CCList.length languages > 1
      then (
        let action =
          assignment_specific_path
            ~suffix:"swap-session/template-language"
            experiment.Experiment.id
            session_id
            assignment_id
        in
        Some
          Htmx.
            [ hx_target ("#" ^ id)
            ; hx_trigger "change"
            ; hx_swap "innerHTML"
            ; hx_get (action |> Sihl.Web.externalize_path)
            ])
      else None
    in
    div
      ~a:[ a_id id ]
      [ Page_admin_message_template.template_inputs
          ?language_select_attriutes
          ~hide_text_message_input:true
          context
          text_messages_disabled
          (`Create swap_session_template)
          Message_template.Label.AssignmentSessionChange
          ~languages
          ?fixed_language:experiment.Experiment.language
          ~selected_language:swap_session_template.Message_template.language
          ~flash_fetcher
      ]
  ;;

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
    text_messages_disabled
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
      let str = start_end_with_duration_human m in
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
                  [ swap_session_notification_form_fields
                      context
                      experiment
                      session.Session.id
                      assignment.id
                      languages
                      swap_session_template
                      flash_fetcher
                      text_messages_disabled
                  ]
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

  let direct_message_modal
    ({ Pool_context.language; csrf; _ } as context)
    ?selected_language
    session
    message_template
    languages
    assignments
    =
    let open Pool_common in
    let experiment = session.Session.experiment in
    let action =
      let open Session in
      HttpUtils.Url.Admin.session_path
        ~suffix:"direct-message/send"
        experiment.Experiment.id
        session.id
      |> Sihl.Web.externalize_path
    in
    let text_message_enabled =
      match assignments with
      | `One { Assignment.contact; _ } ->
        CCOption.is_some contact.Contact.cell_phone
      | `Multiple (_ : Assignment.t list) -> true
    in
    let available_channels =
      MessageChannel.filtered_channels text_message_enabled
    in
    let text_messages_hint =
      match text_message_enabled, assignments with
      | false, _ | _, `One (_ : Assignment.t) -> txt ""
      | true, `Multiple assignments ->
        assignments
        |> CCList.filter_map (fun { Assignment.contact; _ } ->
          match contact.Contact.cell_phone with
          | None -> Some contact
          | Some (_ : Pool_user.CellPhone.t) -> None)
        |> (function
         | [] -> txt ""
         | contacts ->
           let hint =
             contacts
             |> CCList.map (fun contact ->
               li [ Contact.fullname contact |> txt ])
             |> ul
             |> fun list ->
             [ p
                 [ txt
                     (Utils.hint_to_string
                        language
                        I18n.ContactsWithoutCellPhone)
                 ]
             ; list
             ]
             |> Component.Notification.notification language `Error
           in
           let input =
             checkbox_element
               ~as_switch:true
               ~orientation:`Vertical
               ~value:true
               language
               Field.FallbackToEmail
           in
           div
             ~a:[ a_user_data "text-message-hint" ""; a_class [ "stack" ] ]
             [ hint; input ])
    in
    let hidden_inputs =
      assignments
      |> (function
            | `One assignment -> [ assignment ]
            | `Multiple assignments -> assignments)
      |> CCList.map (fun { Assignment.id; _ } ->
        input
          ~a:
            [ a_input_type `Hidden
            ; a_name Field.(array_key Assignment)
            ; a_value (Assignment.Id.value id)
            ]
          ())
      |> div ~a:[ a_class [ "hidden" ] ]
    in
    let scripts =
      Format.asprintf
        {js|
          const channel = "%s";
          const sms = "%s";
          const emailText = "%s";
          const plainText = "%s";

          const modal = document.getElementById("direct-message-modal");

          const channelSelect = modal.querySelector(`[name="${channel}"]`)
          const smsEl = modal.querySelector(`[name="${sms}"]`).closest(".form-group");
          const smsHint = modal.querySelector("[data-text-message-hint]");
          const emailTextEl = modal.querySelector(`[name="${emailText}"]`).closest(".form-group");
          const plainTextEl = modal.querySelector(`[name="${plainText}"]`).closest(".form-group");

          const smsEls = [smsEl];
          if(smsHint) {
            smsEls.push(smsHint)
          }
          
          const emailEls = [emailTextEl, plainTextEl];

          const toggleVisibility = () => {
            if(channelSelect.value === "email") {
              smsEls.forEach(el => {
                el.classList.add("hidden");
              })
              emailEls.forEach(el => {
                el.classList.remove("hidden");
              })
            } else {
              emailEls.forEach(el => {
                el.classList.add("hidden");
              })
              smsEls.forEach(el => {
                el.classList.remove("hidden");
              })
            }
          }

          channelSelect.addEventListener("change", () => {
            toggleVisibility()
          });

          toggleVisibility();
        |js}
        Field.(show MessageChannel)
        Field.(show SmsText)
        Field.(show EmailText)
        Field.(show PlainText)
    in
    let html =
      form
        ~a:[ a_method `Post; a_class [ "stack" ]; a_action action ]
        [ message_channel_select language available_channels
        ; text_messages_hint
        ; Page_admin_message_template.template_inputs
            ~hide_text_message_input:(not text_message_enabled)
            context
            true
            (`Create message_template)
            Message_template.Label.AssignmentSessionChange
            ~languages
            ?fixed_language:experiment.Experiment.language
            ?selected_language
        ; csrf_element csrf ()
        ; hidden_inputs
        ; div
            ~a:[ a_class [ "flexrow"; "justify-end" ] ]
            [ submit_element language Message.(Send None) () ]
        ; script Unsafe.(data scripts)
        ]
    in
    Component.Modal.create
      ~active:true
      language
      (fun lang ->
        Utils.control_to_string lang Message.(Send (Some Field.Message)))
      direct_message_modal_id
      html
  ;;

  let print_assignments_list
    ?(view_contact_name = false)
    ?(view_contact_info = false)
    Pool_context.{ language; _ }
    experiment
    session
    assignments
    =
    let assignemnts_table_id =
      Format.asprintf "assignments-%s" Session.(Id.value session.Session.id)
    in
    let swap_session_modal_id = swap_session_modal_id session in
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
        let right = [ Field.CanceledAt |> field_to_text ] in
        left @ checkboxes @ right
      in
      let rows =
        CCList.map
          (fun (assignment : Assignment.t) ->
            CCList.map snd contact_information
            @ CCList.map snd external_data_field
            @ [ assignment_participated; assignment_no_show; canceled_at ]
            |> CCList.mapi (fun i fcn ->
              let value = fcn assignment in
              if CCInt.equal i 0
              then
                div
                  ~a:[ a_class [ "flexrow"; "flex-gap-sm" ] ]
                  (value :: Status.make_icons language assignment.contact `Name)
              else value))
          assignments
      in
      div
        [ table_legend language
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
        ]
  ;;
end

let data_table
  ?(access_contact_profiles = false)
  ?(send_direct_message = false)
  ?(view_contact_name = false)
  ?(view_contact_info = false)
  ?(is_print = false)
  (Pool_context.{ language; csrf; user; _ } as context)
  experiment
  session
  text_messages_enabled
  ((assignments, custom_fields), query)
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
    Component.DataTable.create_meta
      ?filter:Assignment.filterable_by
      ~search:Assignment.searchable_by
      url
      query
      language
  in
  let conditional_left_columns =
    [ ( view_contact_name
      , Pool_user.column_name
      , fun { contact; _ } ->
          Page_admin_contact.contact_lastname_firstname
            access_contact_profiles
            contact )
    ; view_contact_info, Pool_user.column_email, contact_email
    ; view_contact_info, Contact.column_cell_phone, contact_cellphone
    ]
  in
  let conditional_right_columns =
    [ ( Experiment.(external_data_required_value experiment)
      , column_external_data_id_abbr
      , assignment_external_data_id )
    ]
  in
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
    form
      ~a:
        [ a_action (action assignment suffix |> Sihl.Web.externalize_path)
        ; a_method `Post
        ; a_user_data
            "confirmable"
            Pool_common.(Utils.confirmable_to_string language confirmable)
        ]
      [ csrf_element csrf ()
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
  let session_change_toggle assignment =
    let action =
      action assignment "swap-session"
      |> Sihl.Web.externalize_path
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
            (Format.asprintf "#%s" (swap_session_modal_id session))
        ]
      ~is_text:true
      ~control:(language, Pool_common.Message.ChangeSession)
      ~icon:Component.Icon.SwapHorizonal
  in
  let direct_message_toggle assignment =
    let action =
      HttpUtils.Url.Admin.session_path
        ~suffix:"direct-message"
        experiment.Experiment.id
        session.Session.id
      |> Sihl.Web.externalize_path
    in
    link_as_button
      "#"
      ~attributes:
        Htmx.
          [ hx_trigger "click"
          ; hx_post action
          ; hx_swap "outerHTML"
          ; make_hx_vals
              [ Field.(array_key Assignment), Id.value assignment.id ]
          ; hx_target ("#" ^ direct_message_modal_id)
          ]
      ~is_text:true
      ~control:(language, Pool_common.Message.(Send (Some Field.Message)))
      ~icon:Component.Icon.MailOutline
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
  let has_custom_fields = CCList.is_empty custom_fields |> not in
  let cols =
    let left =
      conditional_left_columns
      |> CCList.filter_map (fun (check, column, _) ->
        if check then Some (`column column) else None)
    in
    let custom_fields_header =
      `custom (Partials.custom_fields_header language custom_fields)
    in
    let center =
      let base = [ `column column_participated; `column column_no_show ] in
      if has_custom_fields then custom_fields_header :: base else base
    in
    let right =
      conditional_right_columns
      |> CCList.filter_map (fun (check, column, _) ->
        if check then Some (`column column) else None)
    in
    let base = left @ center @ right in
    if is_print then base else base @ [ `empty ]
  in
  let th_class =
    let left = [ "w-2"; "w-3"; "w-2" ] in
    let right = [ "w-1"; "w-1"; "w-2" ] in
    if has_custom_fields then left @ [ "w-2" ] @ right else left @ right
  in
  let row (assignment : t) =
    let tr cells =
      let assignment_id = a_user_data "id" (Id.value assignment.id) in
      match assignment.marked_as_deleted |> MarkedAsDeleted.value with
      | true -> tr ~a:[ a_class [ "bg-red-lighter" ]; assignment_id ] cells
      | false -> tr ~a:[ assignment_id ] cells
    in
    let custom_fields =
      Partials.custom_field_cells
        language
        user
        assignment.custom_fields
        custom_fields
    in
    let left =
      conditional_left_columns
      |> CCList.filter_map (fun (check, _, to_html) ->
        if check then Some (to_html assignment) else None)
    in
    let center =
      let base =
        [ assignment_participated assignment; assignment_no_show assignment ]
      in
      if has_custom_fields then custom_fields :: base else base
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
      ; send_direct_message, direct_message_toggle
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
               assignment
               text_messages_enabled)
        | false -> None)
      assignments
    |> function
    | [] -> None
    | modals ->
      Some (div ~a:[ a_class [ "assignment-reminder-modals" ] ] modals)
  in
  Component.DataTable.make
    ~th_class
    ~target_id:"assignments-table"
    ~cols
    ~row
    ?prepend_html:modals
    data_table
    assignments
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
      ; p [ session |> Session.start_end_with_duration_human |> txt ]
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
