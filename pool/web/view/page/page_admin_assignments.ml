open CCFun.Infix
open Tyxml.Html
open Component.Input
module Field = Pool_common.Message.Field

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

type assignment_redirect =
  | Assignments
  | DeletedAssignments
  | Session
[@@deriving show { with_path = false }, yojson]

let read_assignment_redirect m =
  m
  |> Format.asprintf "[\"%s\"]"
  |> Yojson.Safe.from_string
  |> fun json ->
  try Some (assignment_redirect_of_yojson json) with
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
      { Assignment.id; contact; _ }
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
        let timestamps =
          let open Session in
          let format = Component.Utils.format_reminder_sent_opt ~default:"-" in
          [ Field.EmailRemindersSentAt, format session.email_reminder_sent_at
          ; ( Field.TextMessageRemindersSentAt
            , format session.text_message_reminder_sent_at )
          ]
          |> CCList.map (fun (label, value) ->
            li
              [ txt
                  (Pool_common.Utils.field_to_string language label
                   |> CCString.capitalize_ascii)
              ; txt ": "
              ; value
              ])
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
        [ Component.Icon.(to_html Mail); txt (title language) ]
    ;;
  end

  let overview_list
    ?(access_contact_profiles = false)
    ?(view_contact_name = false)
    ?(view_contact_info = false)
    ?(external_data_required = false)
    redirect
    (Pool_context.{ language; csrf; _ } as context)
    experiment_id
    session
    assignments
    =
    let open Pool_common in
    let default = txt "" in
    let deletable = CCFun.(Assignment.is_deletable %> CCResult.is_ok) in
    let cancelable m =
      Session.assignments_cancelable session |> CCResult.is_ok
      && Assignment.is_cancellable m |> CCResult.is_ok
    in
    let action { Assignment.id; _ } suffix =
      assignment_specific_path ~suffix experiment_id session.Session.id id
    in
    let create_reminder_modal =
      Session.reminder_resendable session |> CCResult.is_ok
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
        ~icon:Component.Icon.Create
    in
    let profile_link { Assignment.contact; _ } =
      let action =
        Format.asprintf "/admin/contacts/%s" Contact.(id contact |> Id.value)
      in
      link_as_button
        action
        ~is_text:true
        ~control:(language, Pool_common.Message.OpenProfile)
        ~icon:Component.Icon.Person
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
        Component.Icon.CloseCircle
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
        Component.Icon.Trash
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
          external_data_required
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
        let right = [ Field.CanceledAt |> field_to_text; default ] in
        left @ checkboxes @ right
      in
      let rows, modals =
        let open CCFun in
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
                    (value :: Component.Contacts.status_icons assignment.contact)
                else value)
            in
            let buttons =
              [ true, edit
              ; access_contact_profiles, profile_link
              ; create_reminder_modal, ReminderModal.button context
              ; cancelable assignment, cancel
              ; deletable assignment, mark_as_deleted
              ]
              |> CCList.filter_map (fun (active, form) ->
                if not active then None else Some (form assignment))
              |> Component.ButtonGroup.dropdown
              |> CCList.pure
            in
            let modals =
              match create_reminder_modal with
              | true ->
                ReminderModal.modal context experiment_id session assignment
                :: modals
              | false -> modals
            in
            (base @ buttons) :: rows, modals)
          ([], [])
          assignments
      in
      div
        [ p
            [ Utils.hint_to_string language I18n.SessionCloseLegend
              |> HttpUtils.add_line_breaks
            ]
        ; Component.Table.horizontal_table
            `Striped
            ~align_last_end:true
            ~thead
            rows
        ; div ~a:[ a_class [ "assignment-reminder-modals" ] ] modals
        ]
  ;;

  let grouped_overview_lists
    ?access_contact_profiles
    ?view_contact_name
    ?view_contact_info
    redirect
    (Pool_context.{ language; _ } as context)
    { Experiment.id; external_data_required; _ }
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
              ~external_data_required:
                (Experiment.ExternalDataRequired.value external_data_required)
              redirect
              context
              id
              session
              assignments
          ])
      assignments
    |> div ~a:[ a_class [ "stack-lg" ] ]
  ;;
end

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
            (Component.Contacts.identity
               view_contact_name
               contact
               (Assignment.Id.to_common id)))
         experiment)
;;
