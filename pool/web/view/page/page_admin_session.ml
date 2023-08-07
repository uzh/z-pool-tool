open Tyxml.Html
open Component
open Input
module Message = Pool_common.Message

let session_title (s : Session.t) =
  Pool_common.I18n.SessionDetailTitle (s.Session.start |> Session.Start.value)
;;

let session_path experiment session =
  Format.asprintf
    "/admin/experiments/%s/sessions/%s"
    Experiment.(Id.value experiment.id)
    Session.(session.id |> Id.value)
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
  default_reminder_lead_time
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
            [ div
                [ timespan_picker
                    language
                    Message.Field.LeadTime
                    ~help:I18n.TimeSpanPickerHint
                    ?value:
                      (CCOption.bind session (fun (e : t) ->
                         e.reminder_lead_time
                         |> CCOption.map Reminder.LeadTime.value))
                    ~flash_fetcher
                ; (experiment.Experiment.session_reminder_lead_time
                   |> CCOption.value ~default:default_reminder_lead_time
                   |> fun t ->
                   Utils.text_to_string
                     language
                     (I18n.SessionReminderDefaultLeadTime
                        (t |> Reminder.LeadTime.value))
                   |> txt
                   |> HttpUtils.default_value_style)
                ]
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
                    language
                    Message.(Delete None)
                    ~submit_type:`Error
                    ()
                ]
            else
              submit_element
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
                if Session.is_closable session |> CCResult.is_ok
                then
                  [ Format.asprintf
                      "/admin/experiments/%s/sessions/%s/close"
                      (Experiment.Id.value experiment_id)
                      (Id.value session.id)
                    |> link_as_button
                         ~control:(language, Pool_common.Message.Close None)
                  ]
                else []
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
                      ~a:[ a_class [ "flexrow"; "flex-gap"; "justify-end" ] ]
                      (close_btn
                       @ [ Format.asprintf
                             "/admin/experiments/%s/sessions/%s"
                             (Experiment.Id.value experiment_id)
                             (Id.value session.id)
                           |> link_as_button ~icon:Icon.Eye
                         ; delete_form ()
                         ])
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
    Component.Table.table_color_legend
      language
      Pool_common.I18n.
        [ Closed, "bg-green-lighter"; Canceled, "bg-red-lighter" ]
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
  default_reminder_lead_time
  duplicate_session
  locations
  flash_fetcher
  =
  session_form
    csrf
    language
    experiment
    default_reminder_lead_time
    ?duplicate:duplicate_session
    locations
    ~flash_fetcher
  |> CCList.return
  |> Layout.Experiment.(
       create context (Control Message.(Create (Some Field.Session))) experiment)
;;

let detail
  ?view_contact_name
  ?view_contact_email
  ?view_contact_cellphone
  (Pool_context.{ language; _ } as context)
  experiment
  (session : Session.t)
  participation_tags
  assignments
  =
  let open Pool_common in
  let open Session in
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
        (Format.asprintf
           "/admin/experiments/%s/sessions/%s/%s"
           (Experiment.Id.value experiment.Experiment.id)
           (Id.value session.id)
           url)
      |> CCOption.pure
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
                         (Experiment.Id.value experiment.Experiment.id)
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
        rows @ ([ canceled; closed ] |> CCList.filter_map CCFun.id)
      in
      Table.vertical_table `Striped language ~align_top:true
      @@ CCOption.map_or ~default:rows (CCList.cons' rows) parent
    in
    let links =
      let duplicate =
        let base =
          Format.asprintf
            "/admin/experiments/%s/sessions"
            (Experiment.Id.value experiment.Experiment.id)
        in
        let link =
          match session.follow_up_to with
          | Some parent_session ->
            Format.asprintf
              "%s/%s/follow-up?duplicate_id=%s"
              base
              (Id.value parent_session)
              (Id.value session.id)
          | None ->
            Format.asprintf
              "%s/create/?duplicate_id=%s"
              base
              (Id.value session.id)
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
        |> wrap
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
          ?view_contact_name
          ?view_contact_email
          ?view_contact_cellphone
          Session
          context
          experiment.Experiment.id
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
         (Experiment.Id.value experiment.Experiment.id)
         (Id.value session.id))
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
  default_reminder_lead_time
  (session : Session.t)
  locations
  session_reminder_templates
  sys_languages
  (current_tags, available_tags, experiment_tags)
  flash_fetcher
  =
  let open Message_template in
  let session_path =
    Format.asprintf "%s/%s" (session_path experiment session)
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
          default_reminder_lead_time
          ~session
          locations
          ~flash_fetcher
      ]
  in
  let message_templates_html label list =
    let edit_path m =
      Message_template.prefixed_template_url ~append:"edit" m |> session_path
    in
    let new_path =
      if CCList.is_empty (Message_template.filter_languages sys_languages list)
      then None
      else session_path Label.(prefixed_human_url label) |> CCOption.pure
    in
    div
      [ h2 ~a:[ a_class [ "heading-2" ] ] [ txt (Label.to_human label) ]
      ; Page_admin_message_template.table language list new_path edit_path
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
  default_reminder_lead_time
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
        default_reminder_lead_time
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

let close
  ?(view_contact_name = false)
  ({ Pool_context.language; csrf; _ } as context)
  experiment
  (session : Session.t)
  assignments
  participation_tags
  =
  let open Pool_common in
  let control = Message.(Close (Some Field.Session)) in
  let form =
    let checkbox_element id field =
      div
        [ input
            ~a:
              [ a_input_type `Checkbox
              ; a_name Message.Field.(array_key field)
              ; a_value (id |> Assignment.Id.value)
              ]
            ()
        ]
    in
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
      let link (id, label) =
        span
          ~a:[ a_id id ]
          [ abbr
              ~a:
                [ a_title
                    Pool_common.(
                      Utils.control_to_string language Message.ToggleAll
                      |> CCString.capitalize_ascii)
                ]
              [ txt label ]
          ]
      in
      let thead =
        txt ""
        :: (Utils.field_to_string_capitalized
              language
              Message.Field.ExternalDataId
            |> txt)
        :: ([ "all-no-show", "NS"; "all-participated", "P" ] |> CCList.map link)
      in
      CCList.map
        (fun ({ Assignment.id; contact; external_data_id; _ } : Assignment.t) ->
          let external_data_id_label =
            Format.asprintf
              "%s-%s"
              Message.Field.(ExternalDataId |> show)
              (Assignment.Id.value id)
          in
          let identity =
            if view_contact_name
            then Contact.fullname contact
            else Assignment.Id.value id
          in
          [ div [ strong [ txt identity ] ]
          ; div
              ~a:[ a_class [ "form-group" ] ]
              [ input
                  ~a:
                    ([ a_id external_data_id_label
                     ; a_name external_data_id_label
                     ; a_input_type `Text
                     ; a_value
                         (CCOption.map_or
                            ~default:""
                            Assignment.ExternalDataId.value
                            external_data_id)
                     ]
                     @
                     if Experiment.external_data_required_value experiment
                     then [ a_required () ]
                     else [])
                  ()
              ]
          ; checkbox_element id Message.Field.NoShow
          ; checkbox_element id Message.Field.Participated
          ])
        assignments
      |> Table.horizontal_table ~thead `Striped
      |> fun table ->
      form
        ~a:
          [ a_method `Post
          ; a_class [ "stack" ]
          ; a_action
              (Format.asprintf
                 "/admin/experiments/%s/sessions/%s/close"
                 (Experiment.Id.value experiment.Experiment.id)
                 Session.(Id.value session.id)
               |> Sihl.Web.externalize_path)
          ; a_user_data "detect-unsaved-changes" ""
          ]
        [ tags_html
        ; Input.csrf_element csrf ()
        ; table
        ; div
            ~a:[ a_class [ "flexrow"; "justify-end" ] ]
            [ Input.submit_element language control ~submit_type:`Primary () ]
        ]
    in
    let scripts =
      {js|
        const noShow = document.querySelectorAll('[name="no_show[]"]');
        for (let i = 0; i < noShow.length; i++) {
            let elm = noShow[i];
            let target = document.querySelector(`[name="participated[]"][value="${elm.value}"]`)
            elm.addEventListener("change", () => {
                if (elm.checked) {
                    target.checked = false;
                }
            })
        }

        const participated = document.querySelectorAll('[name="participated[]"]');
        for (let i = 0; i < participated.length; i++) {
            let elm = participated[i];
            let target = document.querySelector(`[name="no_show[]"][value="${elm.value}"]`)
            elm.addEventListener("change", () => {
                if (elm.checked) {
                    target.checked = false;
                }
            })
        }

        const isActive = (elm) => {
            return elm.dataset.active;
        }

        const setToggleState = (elm, state) => {
            if (state) {
                elm.dataset.active = true;
            } else {
                elm.removeAttribute("data-active");
            }
        }

        function toggleColumnValues(elements, value) {
            elements.forEach((elm) => {
                var event = new Event('change');
                elm.checked = value;
                elm.dispatchEvent(event);
            });
        }

        const toggleNoShow = document.getElementById("all-no-show");
        const toggleParticipated = document.getElementById("all-participated");

        toggleNoShow.addEventListener("click", () => {
            const newState = !isActive(toggleNoShow);
            toggleColumnValues(noShow, newState);
            setToggleState(toggleNoShow, newState);
            setToggleState(toggleParticipated, !newState);
        })

        toggleParticipated.addEventListener("click", () => {
            const newState = !isActive(toggleParticipated);
            toggleColumnValues(participated, newState);
            setToggleState(toggleParticipated, newState);
            setToggleState(toggleNoShow, !newState);
        })
      |js}
    in
    div
      [ h4
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
      ; script (Unsafe.data scripts)
      ]
  in
  div
    [ p [ txt (session |> session_title |> Utils.text_to_string language) ]
    ; form
    ]
  |> CCList.return
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
