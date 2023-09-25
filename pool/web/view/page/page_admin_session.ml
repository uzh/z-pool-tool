open Tyxml.Html
open Component
open Input
module Message = Pool_common.Message

let session_title (s : Session.t) =
  Pool_common.I18n.SessionDetailTitle (s.Session.start |> Session.Start.value)
;;

let session_counter_id = "session-counter-table"

let session_path experiment_id session_id =
  Format.asprintf
    "/admin/experiments/%s/sessions/%s"
    Experiment.(Id.value experiment_id)
    Session.(session_id |> Id.value)
;;

let location_select language options selected () =
  let open Pool_location in
  selector
    ~add_empty:true
    ~option_formatter:(fun (l : t) -> l.name |> Name.value)
    ~required:true
    language
    Message.Field.Location
    (fun (l : t) -> l.id |> Id.value)
    options
    selected
    ()
;;

let session_form
  csrf
  language
  (experiment : Experiment.t)
  default_email_reminder_lead_time
  default_text_msg_reminder_lead_time
  ?(session : Session.t option)
  ?(follow_up_to : Session.t option)
  ?(duplicate : Session.t option)
  locations
  ~flash_fetcher
  =
  let open CCFun in
  let open Session in
  let open Pool_common in
  let has_assignments =
    session
    |> CCOption.map_or ~default:false (fun s -> s |> Session.has_assignments)
  in
  let default_value_session =
    (* Prefill the form with values if making a duplicate, editing a session or
       creating a follow up to a parent. The importance is 1. duplicate, 2.
       session editing, 3. follow_up_to *)
    CCOption.(duplicate <+> session <+> follow_up_to)
  in
  let reschedule_hint () =
    match session, has_assignments with
    | Some session, true ->
      let action =
        Format.asprintf
          "/admin/experiments/%s/sessions/%s/reschedule"
          (Experiment.Id.value experiment.Experiment.id)
          Session.(session.id |> Id.value)
        |> Sihl.Web.externalize_path
      in
      div
        ~a:[ a_class [ "full-width" ] ]
        [ p [ txt "There are assignments for this session. Please use the " ]
        ; a
            ~a:[ a_href action ]
            [ txt "form provided to reschedule a session." ]
        ]
    | _ -> txt ""
  in
  let value = CCFun.flip (CCOption.map_or ~default:"") default_value_session in
  let amount fnc = value (fnc %> ParticipantAmount.value %> CCInt.to_string) in
  let action, submit =
    let base =
      Format.asprintf
        "/admin/experiments/%s/sessions"
        (Experiment.Id.value experiment.Experiment.id)
    in
    match session, follow_up_to with
    | None, None -> base, Message.(Create (Some Field.Session))
    | None, Some follow_up_to ->
      ( Format.asprintf
          "%s/%s/follow-up"
          base
          Session.(follow_up_to.id |> Id.value)
      , Message.(Create (Some Field.FollowUpSession)) )
    | Some session, _ ->
      ( Format.asprintf "%s/%s" base Session.(session.id |> Id.value)
      , Message.(Update (Some Field.Session)) )
  in
  let lead_time_group field get_value default_value =
    div
      [ timespan_picker
          language
          field
          ~help:I18n.TimeSpanPickerHint
          ?value:
            CCOption.(
              bind session get_value |> CCOption.map Reminder.LeadTime.value)
          ~flash_fetcher
      ; Utils.text_to_string
          language
          (I18n.SessionReminderDefaultLeadTime
             (default_value |> Reminder.LeadTime.value))
        |> txt
        |> HttpUtils.default_value_style
      ]
  in
  form
    ~a:
      [ a_class [ "stack" ]
      ; a_method `Post
      ; a_action (action |> Sihl.Web.externalize_path)
      ; a_user_data "detect-unsaved-changes" ""
      ]
    [ csrf_element csrf ()
    ; div
        ~a:[ a_class [ "grid-col-2" ] ]
        [ (let value =
             (* Don't want start date filled out in form if creating with
                duplication or follow up *)
             if CCOption.is_some duplicate || CCOption.is_some follow_up_to
             then None
             else
               session |> CCOption.map (fun (s : t) -> s.start |> Start.value)
           in
           date_time_picker_element
             language
             Message.Field.Start
             ~required:true
             ~flash_fetcher
             ?value
             ~warn_past:true
             ~additional_attributes:
               (if has_assignments then [ a_disabled () ] else []))
        ; timespan_picker
            language
            ~required:true
            Message.Field.Duration
            ~help:I18n.TimeSpanPickerHint
            ?value:
              (CCOption.map
                 (fun (s : t) -> s.duration |> Duration.value)
                 session)
            ~flash_fetcher
            ~additional_attributes:
              (if has_assignments then [ a_disabled () ] else [])
        ; reschedule_hint ()
        ; textarea_element
            language
            Message.Field.Description
            ~value:
              (value (fun s ->
                 s.description |> CCOption.map_or ~default:"" Description.value))
            ~flash_fetcher
        ; textarea_element
            language
            Message.Field.Limitations
            ~value:
              (value (fun s ->
                 s.limitations |> CCOption.map_or ~default:"" Limitations.value))
            ~flash_fetcher
        ; location_select
            language
            locations
            (default_value_session |> CCOption.map (fun s -> s.location))
            ()
        ; input_element
            language
            `Number
            Message.Field.MaxParticipants
            ~required:true
            ~value:(amount (fun s -> s.max_participants))
            ~flash_fetcher
        ; input_element
            language
            `Number
            Message.Field.MinParticipants
            ~required:true
            ~value:(amount (fun s -> s.min_participants))
            ~flash_fetcher
        ; input_element
            language
            `Number
            Message.Field.Overbook
            ~required:true
            ~value:(amount (fun s -> s.overbook))
            ~flash_fetcher
        ]
    ; div
        ~a:[ a_class [ "gap-lg" ] ]
        [ h3
            ~a:[ a_class [ "heading-3" ] ]
            [ txt (Utils.text_to_string language I18n.Reminder) ]
        ; div
            ~a:[ a_class [ "grid-col-2" ] ]
            [ lead_time_group
                Message.Field.EmailLeadTime
                (fun (s : t) -> s.email_reminder_lead_time)
                (experiment.Experiment.email_session_reminder_lead_time
                 |> CCOption.value ~default:default_email_reminder_lead_time)
            ; lead_time_group
                Message.Field.TextMessageLeadTime
                (fun (s : t) -> s.text_message_reminder_lead_time)
                (experiment.Experiment.text_message_session_reminder_lead_time
                 |> CCOption.value ~default:default_text_msg_reminder_lead_time
                )
            ]
        ]
    ; div
        ~a:[ a_class [ "flexrow" ] ]
        [ submit_element ~classnames:[ "push" ] language submit () ]
    ]
;;

let reschedule_session
  ({ Pool_context.language; csrf; _ } as context)
  experiment
  (session : Session.t)
  flash_fetcher
  =
  let open Session in
  let open Pool_common in
  let action =
    Format.asprintf
      "/admin/experiments/%s/sessions/%s/reschedule"
      (Experiment.Id.value experiment.Experiment.id)
      Session.(Id.value session.id)
  in
  form
    ~a:
      [ a_class [ "stack" ]
      ; a_method `Post
      ; a_action (action |> Sihl.Web.externalize_path)
      ]
    [ csrf_element csrf ()
    ; date_time_picker_element
        language
        Message.Field.Start
        ~required:true
        ~flash_fetcher
        ~value:(session.start |> Start.value)
        ~disable_past:true
    ; timespan_picker
        language
        ~required:true
        Message.Field.Duration
        ~help:I18n.TimeSpanPickerHint
        ~value:(session.duration |> Duration.value)
        ~flash_fetcher
    ; div
        ~a:[ a_class [ "flexrow" ] ]
        [ submit_element
            ~classnames:[ "push" ]
            language
            Message.(Reschedule (Some Field.Session))
            ()
        ]
    ]
  |> CCList.return
  |> Layout.Experiment.(
       create
         context
         (Control Message.(Reschedule (Some Field.Session)))
         experiment)
;;

let waiting_list_radio_button language session =
  let open Pool_common in
  if Session.is_fully_booked session
  then span [ txt (Utils.error_to_string language Message.SessionFullyBooked) ]
  else if CCOption.is_some session.Session.follow_up_to
  then
    span
      [ txt
          (Utils.error_to_string language Message.SessionRegistrationViaParent)
      ]
  else (
    match Session.assignment_creatable session |> CCResult.is_ok with
    | false -> txt ""
    | true ->
      input
        ~a:
          [ a_input_type `Radio
          ; a_name Message.Field.(show Session)
          ; a_value Session.(session.id |> Id.value)
          ]
        ())
;;

let session_list
  layout
  Pool_context.{ language; csrf; _ }
  experiment_id
  grouped_sessions
  chronological
  =
  let open Pool_common in
  let follow_up_icon () =
    span
      ~a:[ a_class [ "font-bold" ] ]
      [ abbr
          ~a:
            [ a_title
                (Utils.field_to_string language Message.Field.FollowUpSession)
            ]
          [ txt "(F)" ]
      ]
  in
  let chronological_id = "chronological-sessions" in
  let add_session_btn =
    link_as_button
      ~style:`Success
      ~icon:Icon.Add
      ~classnames:[ "small" ]
      ~control:(language, Message.(Add (Some Field.Session)))
      (Format.asprintf
         "/admin/experiments/%s/sessions/create"
         (experiment_id |> Experiment.Id.value))
  in
  let rows =
    CCList.flat_map
      (fun (parent, follow_ups) ->
        let open Session in
        let session_row session follow_ups =
          let delete_form () =
            if Session.is_deletable session follow_ups |> CCResult.is_ok
            then
              form
                ~a:
                  [ a_method `Post
                  ; a_action
                      (Format.asprintf
                         "/admin/experiments/%s/sessions/%s/delete"
                         (Experiment.Id.value experiment_id)
                         (Id.value session.id)
                       |> Sihl.Web.externalize_path)
                  ; a_user_data
                      "confirmable"
                      (Utils.confirmable_to_string language I18n.DeleteSession)
                  ]
                [ csrf_element csrf ()
                ; submit_element
                    ~has_icon:Icon.Trash
                    ~is_text:true
                    language
                    Message.(Delete None)
                    ~submit_type:`Error
                    ()
                ]
            else
              submit_element
                ~has_icon:Icon.Trash
                ~is_text:true
                language
                Message.(Delete None)
                ~submit_type:`Disabled
                ()
          in
          let key_figures =
            let open Session in
            let value = ParticipantAmount.value in
            Format.asprintf
              "%i / %i (%i)"
              (session.min_participants |> value)
              (session.max_participants |> value)
              (session.overbook |> value)
          in
          let row_attrs =
            let id = a_user_data "id" Session.(Id.value session.id) in
            let classnames =
              let check opt classname =
                if CCOption.is_some opt then Some classname else None
              in
              [ check session.canceled_at "bg-red-lighter"
              ; check session.closed_at "bg-green-lighter"
              ]
              |> CCList.filter_map CCFun.id
            in
            session.follow_up_to
            |> CCOption.map (fun parent ->
              a_user_data "parent-id" (Session.Id.value parent))
            |> CCOption.map_or ~default:[ id ] (fun parent -> [ id; parent ])
            |> fun attrs -> a_class classnames :: attrs
          in
          let title =
            let date = span [ txt (session |> session_date_to_human) ] in
            match CCOption.is_some session.follow_up_to, chronological with
            | false, true | false, false -> date
            | true, true ->
              div
                ~a:[ a_class [ "flexrow"; "flex-gap-sm" ] ]
                [ date; follow_up_icon () ]
            | true, false -> div ~a:[ a_class [ "inset"; "left" ] ] [ date ]
          in
          let base = [ title ] in
          let cells =
            match layout with
            | `SessionOverview ->
              let close_btn =
                Format.asprintf
                  "/admin/experiments/%s/sessions/%s/close"
                  (Experiment.Id.value experiment_id)
                  (Id.value session.id)
                |> link_as_button
                     ~icon:Icon.Create
                     ~is_text:true
                     ~control:(language, Message.Close (Some Field.Session))
              in
              let detail_button =
                Format.asprintf
                  "/admin/experiments/%s/sessions/%s"
                  (Experiment.Id.value experiment_id)
                  (Id.value session.id)
                |> link_as_button
                     ~is_text:true
                     ~icon:Icon.Eye
                     ~control:(language, Message.SessionDetails)
              in
              let buttons =
                [ Session.is_closable session |> CCResult.is_ok, close_btn
                ; true, detail_button
                ; true, delete_form ()
                ]
                |> CCList.filter_map (fun (condition, button) ->
                  if condition then Some button else None)
              in
              let cells =
                Session.
                  [ txt
                      (CCInt.to_string
                         (session.assignment_count |> AssignmentCount.value))
                  ; txt
                      (if CCOption.is_some session.closed_at
                       then
                         session.no_show_count
                         |> NoShowCount.value
                         |> CCInt.to_string
                       else "")
                  ; txt
                      (if CCOption.is_some session.closed_at
                       then
                         session.participant_count
                         |> ParticipantCount.value
                         |> CCInt.to_string
                       else "")
                  ; txt key_figures
                  ; div
                      ~a:[ a_class [ "flexrow" ] ]
                      [ Component.ButtonGroup.dropdown
                          ~classnames:[ "push" ]
                          buttons
                      ]
                  ]
              in
              base @ cells
            | `WaitingList ->
              let cells =
                [ waiting_list_radio_button language session
                ; txt
                    (CCInt.to_string
                       (session.assignment_count |> AssignmentCount.value))
                ; txt key_figures
                ]
              in
              base @ cells
          in
          cells |> CCList.map CCFun.(CCList.return %> td) |> tr ~a:row_attrs
        in
        session_row parent follow_ups
        :: CCList.map CCFun.(flip session_row []) follow_ups)
      grouped_sessions
  in
  let thead =
    let key_figures_head = "Min / Max (Overbook)" in
    let open Message in
    let base = [ Field.Date ] |> Table.fields_to_txt language in
    let cells =
      match layout with
      | `SessionOverview ->
        base
        @ ([ Field.AssignmentCount; Field.NoShowCount; Field.ParticipantCount ]
           |> Table.fields_to_txt language)
        @ [ txt key_figures_head; add_session_btn ]
      | `WaitingList ->
        let to_txt = Table.field_to_txt language in
        base @ [ txt ""; Field.AssignmentCount |> to_txt; txt key_figures_head ]
    in
    cells |> Component.Table.table_head
  in
  let table =
    let id = if chronological then [ a_id chronological_id ] else [] in
    table
      ~a:([ a_class [ "table"; "striped"; "align-last-end" ] ] @ id)
      ~thead
      rows
  in
  let table_legend =
    let open Pool_common in
    let to_string = Utils.text_to_string language in
    let open Component.Table in
    table_legend
      I18n.
        [ to_string Closed, legend_color_item "bg-green-lighter"
        ; to_string Canceled, legend_color_item "bg-red-lighter"
        ]
  in
  let hover_script =
    match chronological with
    | false -> txt ""
    | true ->
      let js =
        {js|
          const highlight = "highlighted";

          document.addEventListener("DOMContentLoaded", () => {
            const table = document.getElementById("chronological-sessions");
            const toggleClass = (e) => {
              const { id, parentId } = e.currentTarget.dataset;
              if (parentId) {
                table
                  .querySelector(`[data-id='${parentId}']`)
                  .classList.toggle(highlight);
              } else {
                table.querySelectorAll(`[data-parent-id='${id}']`).forEach((tr) => {
                  tr.classList.toggle(highlight);
                });
              }
              e.currentTarget.classList.toggle(highlight);
            };
            table.querySelectorAll("tbody tr").forEach((row) => {
              row.addEventListener("mouseenter", (e) => {
                toggleClass(e);
              });
              row.addEventListener("mouseleave", (e) => {
                toggleClass(e);
              });
            });
          });
      |js}
      in
      script (Unsafe.data js)
  in
  div
    ~a:[ a_class [ "stack" ] ]
    [ p [ I18n.SessionIndent |> Utils.text_to_string language |> txt ]
    ; a
        ~a:
          [ a_href
              (if chronological
               then "?"
               else
                 Format.asprintf "?%s=true" Message.Field.(show Chronological))
          ]
        [ (if chronological
           then I18n.SwitchGrouped
           else I18n.SwitchChronological)
          |> Utils.text_to_string language
          |> txt
        ]
      (* TODO [aerben] allow tables to be sorted generally? *)
    ; (if chronological
       then
         p
           [ txt "Sessions marked with "
           ; follow_up_icon ()
           ; txt " are follow-up sessions."
           ]
       else txt "")
    ; table_legend
    ; table
    ; hover_script
    ]
;;

let index
  context
  ({ Experiment.id; _ } as experiment)
  grouped_sessions
  chronological
  =
  let open Pool_common in
  session_list `SessionOverview context id grouped_sessions chronological
  |> CCList.return
  |> Layout.Experiment.(
       create
         ~active_navigation:I18n.Sessions
         ~hint:I18n.ExperimentSessions
         context
         (NavLink I18n.Sessions)
         experiment)
;;

let new_form
  ({ Pool_context.language; csrf; _ } as context)
  experiment
  default_email_reminder_lead_time
  default_text_msg_reminder_lead_time
  duplicate_session
  locations
  flash_fetcher
  =
  session_form
    csrf
    language
    experiment
    default_email_reminder_lead_time
    default_text_msg_reminder_lead_time
    ?duplicate:duplicate_session
    locations
    ~flash_fetcher
  |> CCList.return
  |> Layout.Experiment.(
       create context (Control Message.(Create (Some Field.Session))) experiment)
;;

let detail
  ?access_contact_profiles
  ?view_contact_name
  ?view_contact_info
  (Pool_context.{ language; csrf; _ } as context)
  experiment
  (session : Session.t)
  participation_tags
  assignments
  =
  let open Pool_common in
  let open Session in
  let experiment_id = experiment.Experiment.id in
  let session_id = Session.Id.value session.id in
  let session_link ?style (show, url, control) =
    let style, icon =
      style |> CCOption.map_or ~default:(`Primary, None) CCFun.id
    in
    match show with
    | false -> None
    | true ->
      link_as_button
        ~control:(language, control)
        ~classnames:[ "small" ]
        ~style
        ?icon
        (Format.asprintf "%s/%s" (session_path experiment_id session.id) url)
      |> CCOption.pure
  in
  let resend_reminders_modal =
    let open Pool_common.Reminder in
    if Session.reminder_resendable session |> CCResult.is_ok |> not
    then txt ""
    else (
      let modal_id = "resend-reminders-modal" in
      let resend_txt language =
        Pool_common.(Utils.text_to_string language I18n.ResendReminders)
      in
      let inner =
        let resend_action =
          Format.asprintf
            "%s/resend-reminders"
            (session_path experiment_id session.id)
          |> Sihl.Web.externalize_path
        in
        let warning =
          match
            CCOption.is_none session.email_reminder_sent_at
            && CCOption.is_none session.text_message_reminder_sent_at
          with
          | false -> txt ""
          | true ->
            Notification.notification
              language
              `Warning
              [ Utils.hint_to_string language I18n.ResendRemindersWarning
                |> HttpUtils.add_line_breaks
              ]
        in
        div
          [ warning
          ; p
              [ txt
                  Pool_common.(
                    Utils.hint_to_string language I18n.ResendRemindersChannel)
              ]
          ; form
              ~a:[ a_method `Post; a_action resend_action; a_class [ "stack" ] ]
              [ csrf_element csrf ()
              ; selector
                  language
                  Message.Field.MessageChannel
                  Channel.show
                  Channel.all
                  None
                  ~option_formatter:(fun channel ->
                    Channel.show channel
                    |> CCString.replace ~sub:"_" ~by:" "
                    |> CCString.capitalize_ascii)
                  ()
              ; submit_element language Message.(Resend None) ()
              ]
          ]
      in
      let modal = Modal.create language resend_txt modal_id inner in
      let button =
        a
          ~a:
            [ a_href "#"
            ; a_user_data "modal" modal_id
            ; a_class [ "has-icon"; "primary"; "btn"; "small" ]
            ]
          [ txt (resend_txt language) ]
      in
      div [ modal; button ])
  in
  let session_overview =
    let table =
      let open Message in
      let parent =
        CCOption.map
          (fun follow_up_to ->
            ( Field.MainSession
            , a
                ~a:
                  [ a_href
                      (Format.asprintf
                         "/admin/experiments/%s/sessions/%s"
                         (Experiment.Id.value experiment_id)
                         (Id.value follow_up_to)
                       |> Sihl.Web.externalize_path)
                  ]
                [ Message.Show
                  |> Utils.control_to_string language
                  |> CCString.capitalize_ascii
                  |> txt
                ] ))
          session.follow_up_to
      in
      let counters =
        let length lst = lst |> CCList.length |> CCInt.to_string |> txt in
        let open Assignment in
        let assignment_count =
          ( Field.AssignmentCount
          , session.assignment_count
            |> AssignmentCount.value
            |> CCInt.to_string
            |> txt )
        in
        match session.Session.closed_at with
        | None -> [ assignment_count ]
        | Some (_ : Ptime.t) ->
          assignment_count
          :: [ ( Field.NoShowCount
               , assignments
                 |> CCList.filter (fun { no_show; _ } ->
                   no_show |> CCOption.map_or ~default:false NoShow.value)
                 |> length )
             ; ( Field.Participated
               , assignments
                 |> CCList.filter (fun { participated; _ } ->
                   participated
                   |> CCOption.map_or ~default:false Participated.value)
                 |> length )
             ]
      in
      let rows =
        let amount amt = amt |> ParticipantAmount.value |> string_of_int in
        [ ( Field.Start
          , session.start
            |> Start.value
            |> Utils.Time.formatted_date_time
            |> txt )
        ; ( Field.Duration
          , session.duration
            |> Duration.value
            |> Utils.Time.formatted_timespan
            |> txt )
        ; ( Field.Description
          , CCOption.map_or ~default:"" Description.value session.description
            |> Http_utils.add_line_breaks )
        ; ( Field.Limitations
          , CCOption.map_or ~default:"" Limitations.value session.limitations
            |> Http_utils.add_line_breaks )
        ; ( Field.Location
          , Partials.location_to_html language session.Session.location )
        ; Field.MaxParticipants, amount session.max_participants |> txt
        ; Field.MinParticipants, amount session.min_participants |> txt
        ; Field.Overbook, amount session.overbook |> txt
        ]
        |> fun rows ->
        let canceled =
          session.canceled_at
          |> CCOption.map (fun c ->
            Field.CanceledAt, Utils.Time.formatted_date_time c |> txt)
        in
        let closed =
          session.closed_at
          |> CCOption.map (fun c ->
            Field.ClosedAt, Utils.Time.formatted_date_time c |> txt)
        in
        let time_stamps =
          let format = Component.Utils.format_reminder_sent_opt in
          [ Field.EmailRemindersSentAt, format session.email_reminder_sent_at
          ; ( Field.TextMessageRemindersSentAt
            , format session.text_message_reminder_sent_at )
          ]
        in
        rows
        @ counters
        @ time_stamps
        @ ([ canceled; closed ] |> CCList.filter_map CCFun.id)
      in
      Table.vertical_table `Striped language ~align_top:true
      @@ CCOption.map_or ~default:rows (CCList.cons' rows) parent
    in
    let links =
      let duplicate =
        let base =
          Format.asprintf
            "/admin/experiments/%s/sessions"
            (Experiment.Id.value experiment_id)
        in
        let link =
          match session.follow_up_to with
          | Some parent_session ->
            Format.asprintf
              "%s/%s/follow-up?duplicate_id=%s"
              base
              (Id.value parent_session)
              session_id
          | None -> Format.asprintf "%s/create/?duplicate_id=%s" base session_id
        in
        link_as_button
          ~control:(language, Message.Duplicate (Some Field.Session))
          ~classnames:[ "small" ]
          link
      in
      let wrap = div ~a:[ a_class [ "flexrow"; "flex-gap" ] ] in
      let right =
        Message.
          [ ( CCOption.is_none session.follow_up_to
            , "follow-up"
            , Create (Some Field.FollowUpSession) )
          ]
        |> CCList.filter_map session_link
        |> CCList.cons duplicate
        |> wrap
      in
      let left =
        Message.
          [ ( ( session.assignment_count |> AssignmentCount.value > 0
                && CCOption.is_none session.closed_at
              , "reschedule"
              , Reschedule (Some Field.Session) )
            , None )
          ; ( ( session |> is_closable |> CCResult.is_ok
              , "close"
              , Close (Some Field.Session) )
            , None )
          ; ( ( session |> is_cancellable |> CCResult.is_ok
              , "cancel"
              , Cancel (Some Field.Session) )
            , Some (`Error, Some Icon.CloseCircle) )
          ]
        |> CCList.filter_map (fun (t, style) -> session_link ?style t)
        |> fun btns -> wrap (resend_reminders_modal :: btns)
      in
      div
        ~a:[ a_class [ "flexrow"; "flex-gap"; "justify-between" ] ]
        [ left; right ]
    in
    div ~a:[ a_class [ "stack" ] ] [ table; links ]
  in
  let tags_html =
    div
      [ h2
          ~a:[ a_class [ "heading-2" ] ]
          Pool_common.[ Utils.nav_link_to_string language I18n.Tags |> txt ]
      ; Component.Tag.tag_list language participation_tags
      ]
  in
  let assignments_html =
    let assignment_list =
      Page_admin_assignments.(
        Partials.overview_list
          ?access_contact_profiles
          ?view_contact_name
          ?view_contact_info
          Session
          context
          experiment
          session
          assignments)
    in
    div
      [ h2
          ~a:[ a_class [ "heading-2" ] ]
          [ txt (Utils.nav_link_to_string language I18n.Assignments) ]
      ; assignment_list
      ]
  in
  let edit_button =
    link_as_button
      ~icon:Icon.Create
      ~classnames:[ "small" ]
      ~control:(language, Message.(Edit (Some Field.Session)))
      (Format.asprintf
         "/admin/experiments/%s/sessions/%s/edit"
         (Experiment.Id.value experiment_id)
         session_id)
  in
  div
    ~a:[ a_class [ "stack-lg" ] ]
    [ session_overview; tags_html; assignments_html ]
  |> CCList.return
  |> Layout.Experiment.(
       create
         ~buttons:edit_button
         context
         (I18n (session_title session))
         experiment)
;;

let edit
  ({ Pool_context.language; csrf; _ } as context)
  experiment
  default_email_reminder_lead_time
  default_text_msg_reminder_lead_time
  (session : Session.t)
  locations
  session_reminder_templates
  sys_languages
  (current_tags, available_tags, experiment_tags)
  flash_fetcher
  =
  let open Message_template in
  let session_path =
    Format.asprintf
      "%s/%s"
      (session_path experiment.Experiment.id session.Session.id)
  in
  let form =
    div
      [ p
          [ txt
              (session
               |> session_title
               |> Pool_common.Utils.text_to_string language)
          ]
      ; session_form
          csrf
          language
          experiment
          default_email_reminder_lead_time
          default_text_msg_reminder_lead_time
          ~session
          locations
          ~flash_fetcher
      ]
  in
  let message_templates_html label list =
    let build_path append =
      CCFun.(Message_template.prefixed_template_url ~append %> session_path)
    in
    let edit_path = build_path "edit" in
    let delete_path = build_path "delete", csrf in
    let buttons =
      if CCList.is_empty (Message_template.filter_languages sys_languages list)
      then None
      else
        session_path Label.(prefixed_human_url label)
        |> Page_admin_message_template.build_add_button label
        |> CCOption.pure
    in
    div
      [ h2 ~a:[ a_class [ "heading-2" ] ] [ txt (Label.to_human label) ]
      ; Page_admin_message_template.table
          ?buttons
          ~delete_path
          language
          list
          edit_path
      ]
  in
  let tags_html =
    let tags_action =
      Format.asprintf
        "%s/%s"
        (session_path Message.Field.(human_url ParticipationTag))
    in
    let remove_action (tag : Tags.t) =
      Format.asprintf "%s/%s" Tags.(tag.id |> Id.value) "remove" |> tags_action
    in
    div
      [ h3
          ~a:[ a_class [ "heading-3" ] ]
          [ txt Pool_common.(Utils.nav_link_to_string language I18n.Tags) ]
      ; p
          Pool_common.
            [ Utils.hint_to_string language I18n.ParticipationTags |> txt ]
      ; div
          ~a:[ a_class [ "switcher-lg"; "flex-gap" ] ]
          [ Tag.add_tags_form
              context
              ~existing:current_tags
              available_tags
              (tags_action "assign")
          ; div
              ~a:[ a_class [ "flexcolumn"; "flex-gap" ] ]
              [ Component.Tag.tag_list
                  language
                  ~remove_action:(remove_action, csrf)
                  ~title:Pool_common.I18n.SelectedTags
                  current_tags
              ; div
                  ~a:
                    [ a_class [ "border-top"; "inset"; "vertical"; "n-shape" ] ]
                  [ p
                      [ txt
                          "Every participant of a session of this experiment \
                           will be tagged with the following tags by default."
                      ]
                  ; Component.Tag.tag_list language experiment_tags
                  ]
              ]
          ]
      ]
  in
  div
    ~a:[ a_class [ "stack-lg" ] ]
    [ form
    ; message_templates_html Label.SessionReminder session_reminder_templates
    ; tags_html
    ]
  |> CCList.return
  |> Layout.Experiment.(
       create context (Control Message.(Edit (Some Field.Session))) experiment)
;;

let follow_up
  ({ Pool_context.language; csrf; _ } as context)
  experiment
  default_email_reminder_lead_time
  default_text_msg_reminder_lead_time
  duplicate_session
  (parent_session : Session.t)
  locations
  flash_fetcher
  =
  let open Pool_common in
  div
    [ p
        [ txt
            Utils.(
              parent_session
              |> session_title
              |> text_to_string language
              |> CCFormat.asprintf
                   "%s %s"
                   (I18n.FollowUpSessionFor |> text_to_string language))
        ]
    ; session_form
        csrf
        language
        experiment
        default_email_reminder_lead_time
        default_text_msg_reminder_lead_time
        ~follow_up_to:parent_session
        ?duplicate:duplicate_session
        locations
        ~flash_fetcher
    ]
  |> CCList.return
  |> Layout.Experiment.(
       create
         context
         (Control Message.(Create (Some Field.FollowUpSession)))
         experiment)
;;

let session_counters
  language
  { Assignment.total; num_no_shows; num_participations }
  =
  let field_to_string field =
    Pool_common.Utils.field_to_string language field
    |> CCString.capitalize_ascii
    |> txt
  in
  div
    ~a:
      [ a_class [ "flexrow"; "flex-gap"; "inset-sm" ]
      ; a_id session_counter_id
      ; a_user_data "hx-swap-oob" "true"
      ]
    [ div
        [ strong
            [ field_to_string Field.Total
            ; txt ":"
            ; txt " "
            ; CCInt.to_string total |> txt
            ]
        ]
    ; div
        ~a:[ a_class [ "push"; "session-close-checkboxes" ] ]
        [ div [ strong [ CCInt.to_string num_participations |> txt ] ]
        ; div [ strong [ CCInt.to_string num_no_shows |> txt ] ]
        ]
    ]
;;

let close_assignment_htmx_row
  { Pool_context.language; csrf; _ }
  (experiment : Experiment.t)
  ~view_contact_name
  ?(updated_fields = [])
  session
  ?counters
  ({ Assignment.id; no_show; participated; external_data_id; contact; _ } as
   assignment)
  =
  let open Assignment in
  let open Pool_common.Utils in
  let errors =
    validate experiment assignment
    |> function
    | Ok () -> None
    | Error err -> Some err
  in
  let session_path = session_path experiment.Experiment.id session.Session.id in
  let checkbox_element field value =
    let checked = if value then [ a_checked () ] else [] in
    let classnames =
      if CCList.mem ~eq:Pool_common.Message.Field.equal field updated_fields
      then [ a_class [ "success" ] ]
      else []
    in
    div
      ~a:[ a_class [ "form-group" ] ]
      [ div
          [ input
              ~a:
                ([ a_input_type `Checkbox; a_name (Field.show field) ]
                 @ checked
                 @ classnames)
              ()
          ]
      ]
  in
  let default_bool fnc = CCOption.map_or ~default:false fnc in
  let identity =
    Component.Contacts.identity
      view_contact_name
      contact
      (Assignment.Id.to_common id)
  in
  let action =
    Format.asprintf "%s/assignments/%s/close" session_path (Id.value id)
    |> Sihl.Web.externalize_path
  in
  let external_data_field =
    let open Experiment in
    match experiment.external_data_required |> ExternalDataRequired.value with
    | false -> txt ""
    | true ->
      let value =
        CCOption.map_or
          ~default:""
          Assignment.ExternalDataId.value
          external_data_id
      in
      let field = Field.ExternalDataId in
      let classnames =
        let open Pool_common.Message in
        if CCList.mem
             ~eq:equal_error
             (FieldRequired Field.ExternalDataId)
             (errors |> CCOption.value ~default:[])
        then [ a_class [ "has-error" ] ]
        else if CCList.mem
                  ~eq:Pool_common.Message.Field.equal
                  field
                  updated_fields
        then [ a_class [ "success" ] ]
        else []
      in
      div
        ~a:[ a_class [ "form-group"; "flex-basis-30" ] ]
        [ input
            ~a:
              ([ a_input_type `Text
               ; a_value value
               ; a_name Field.(show field)
               ; a_placeholder
                   (field_to_string language field |> CCString.capitalize_ascii)
               ]
               @ classnames)
            ()
        ]
  in
  let errors =
    errors
    |> CCOption.map_or ~default:(txt "") (fun errors ->
      let error_to_item err =
        error_to_string language err |> txt |> CCList.return |> li
      in
      CCList.map error_to_item errors |> ul ~a:[ a_class [ "color-red" ] ])
  in
  form
    ~a:
      [ a_user_data "hx-post" action
      ; a_user_data "hx-trigger" "change"
      ; a_user_data "hx-swap" "outerHTML"
      ; a_user_data "assignment" (Id.value id)
      ; a_class [ "flexcolumn"; "stack-sm"; "inset-sm" ]
      ]
    [ div
        ~a:[ a_class [ "flexrow"; "flex-gap-sm" ] ]
        [ csrf_element csrf ()
        ; div
            ~a:
              [ a_class
                  [ "grow"
                  ; "flexrow"
                  ; "flex-gap-sm"
                  ; "flexcolumn-mobile"
                  ; "justify-between"
                  ]
              ]
            [ strong [ txt identity ]; external_data_field ]
        ; div
            ~a:[ a_class [ "session-close-checkboxes" ] ]
            [ checkbox_element
                Message.Field.Participated
                (default_bool Participated.value participated)
            ; checkbox_element
                Message.Field.NoShow
                (default_bool NoShow.value no_show)
            ]
        ]
    ; errors
    ; counters |> CCOption.map_or ~default:(txt "") (session_counters language)
    ]
;;

let close
  ?(view_contact_name = false)
  ({ Pool_context.language; csrf; _ } as context)
  experiment
  (session : Session.t)
  assignments
  participation_tags
  counters
  =
  let open Pool_common in
  let control = Message.(Close (Some Field.Session)) in
  let session_path = session_path experiment.Experiment.id session.Session.id in
  let tags_html =
    let participation_tags_list =
      match participation_tags with
      | [] ->
        Utils.hint_to_string
          language
          I18n.SessionCloseNoParticipationTagsSelected
        |> txt
      | tags ->
        let tags = Component.Tag.tag_list language tags in
        div
          [ p
              [ Utils.hint_to_string
                  language
                  I18n.SessionCloseParticipationTagsSelected
                |> txt
              ]
          ; tags
          ]
    in
    div
      [ h4
          ~a:[ a_class [ "heading-4" ] ]
          [ txt (Utils.nav_link_to_string language I18n.Tags) ]
      ; participation_tags_list
      ]
  in
  let table =
    match assignments with
    | [] ->
      p
        [ txt
            Pool_common.(Utils.text_to_string language I18n.AssignmentListEmpty)
        ]
    | assignments ->
      CCList.map
        (close_assignment_htmx_row
           context
           experiment
           ~view_contact_name
           session)
        assignments
      |> div ~a:[ a_class [ "flexcolumn"; "striped" ] ]
      |> fun table ->
      let header =
        div
          ~a:[ a_class [ "flexrow"; "inset-sm" ] ]
          [ div
              ~a:[ a_class [ "push"; "session-close-checkboxes" ] ]
              [ div [ strong [ txt "P" ] ]; div [ strong [ txt "NS" ] ] ]
          ]
      in
      let counters = session_counters language counters in
      let scripts =
        Format.asprintf
          {js|
            const noShow = "%s";
            const participated = "%s";

            const forms = document.querySelectorAll("form[data-assignment]");

            document.addEventListener('htmx:beforeRequest', (e) => {
              const form = e.detail.target;
              const trigger = e.detail.requestConfig.triggeringEvent.srcElement;
              switch (trigger.name) {
                case noShow:
                  if(trigger.checked) {
                    e.detail.requestConfig.parameters['participated'] = false
                  }
                  break;
                case participated:
                  if(trigger.checked) {
                    e.detail.requestConfig.parameters[noShow] = false
                  }
                  break;
                default:
                  return;
              }
            });
          |js}
          Field.(show NoShow)
          Field.(show Participated)
      in
      div [ header; table; counters; script (Unsafe.data scripts) ]
  in
  let submit_session_close =
    form
      ~a:
        [ a_method `Post
        ; a_class [ "stack" ]
        ; a_action
            (Format.asprintf "%s/close" session_path
             |> Sihl.Web.externalize_path)
        ; a_user_data "detect-unsaved-changes" ""
        ]
      [ Input.csrf_element csrf ()
      ; div
          ~a:[ a_class [ "flexrow"; "justify-end" ] ]
          [ Input.submit_element language control ~submit_type:`Primary () ]
      ]
  in
  [ div
      [ p [ txt (session |> session_title |> Utils.text_to_string language) ]
      ; tags_html
      ; h4
          ~a:[ a_class [ "heading-4" ] ]
          [ txt
              (Utils.field_to_string language Message.Field.Participants
               |> CCString.capitalize_ascii)
          ]
      ; p
          [ Utils.hint_to_string language I18n.SessionCloseLegend
            |> HttpUtils.add_line_breaks
          ]
      ; p
          [ Utils.hint_to_string language I18n.SessionCloseHints |> Unsafe.data
          ]
      ; table
      ; submit_session_close
      ]
  ]
  |> Layout.Experiment.(create context (Control control) experiment)
;;

let cancel
  ({ Pool_context.language; csrf; _ } as context)
  experiment
  (session : Session.t)
  follow_ups
  flash_fetcher
  =
  let open Pool_common in
  let open CCFun in
  let action =
    Format.asprintf
      "/admin/experiments/%s/sessions/%s/cancel"
      (Experiment.Id.value experiment.Experiment.id)
      Session.(Id.value session.id)
  in
  let follow_ups_notification () =
    match follow_ups with
    | [] -> txt ""
    | follow_ups ->
      let follow_ups =
        follow_ups
        |> CCList.map (fun session ->
          session_title session
          |> Utils.text_to_string language
          |> txt
          |> CCList.return
          |> li)
        |> ul ~a:[ a_class [ "gap-xs" ] ]
      in
      [ Utils.hint_to_string language I18n.SessionCancellationWithFollowups
        |> HttpUtils.add_line_breaks
      ; follow_ups
      ]
      |> Component.Notification.notification language `Error
  in
  (match Session.is_cancellable session with
   | Error reason -> p [ reason |> Utils.error_to_string language |> txt ]
   | Ok _ ->
     div
       ~a:[ a_class [ "stack" ] ]
       [ follow_ups_notification ()
       ; form
           ~a:
             [ a_class [ "stack" ]
             ; a_method `Post
             ; a_action (action |> Sihl.Web.externalize_path)
             ]
           [ csrf_element csrf ()
           ; textarea_element
               ~help:I18n.SessionCancelMessage
               ~flash_fetcher
               ~required:true
               language
               Message.Field.Reason
           ; notify_via_selection language
           ; div
               ~a:[ a_class [ "flexrow" ] ]
               [ submit_element
                   ~classnames:[ "push" ]
                   language
                   Message.(Cancel (Some Field.Session))
                   ()
               ]
           ]
       ])
  |> CCList.return
  |> Layout.Experiment.(
       create context (Control Message.(Cancel (Some Field.Session))) experiment)
;;

let message_template_form
  ({ Pool_context.language; _ } as context)
  tenant
  experiment
  session
  languages
  label
  template
  flash_fetcher
  =
  let open Message_template in
  let action =
    let go =
      Format.asprintf
        "/admin/experiments/%s/sessions/%s/%s"
        Experiment.(Id.value experiment.Experiment.id)
        Session.(Id.value session.Session.id)
    in
    match template with
    | None -> go (Label.prefixed_human_url label)
    | Some template -> prefixed_template_url template |> go
  in
  let title =
    let open Pool_common in
    (match template with
     | None -> Message.(Create None)
     | Some _ -> Message.(Edit None))
    |> fun control ->
    Layout.Experiment.Text
      (Format.asprintf
         "%s %s"
         (control |> Utils.control_to_string language)
         (label |> Label.to_human |> CCString.lowercase_ascii))
  in
  let text_elements =
    Component.MessageTextElements.message_template_help
      ~experiment
      ~session
      language
      tenant
      label
  in
  Page_admin_message_template.template_form
    context
    ~text_elements
    ?languages
    template
    action
    flash_fetcher
  |> CCList.return
  |> Layout.Experiment.create context title experiment
;;
