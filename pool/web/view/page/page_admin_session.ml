open Tyxml.Html
open Component
open Input
open CCFun.Infix
module Message = Pool_common.Message

let session_title (s : Session.t) =
  Pool_common.I18n.SessionDetailTitle (s.Session.start |> Session.Start.value)
;;

let session_counter_id = "session-counter-table"

let follow_up_icon language =
  span
    ~a:[ a_class [ "font-bold" ] ]
    [ abbr
        ~a:
          [ a_title
              Pool_common.(
                Utils.field_to_string language Message.Field.FollowUpSession)
          ]
        [ txt "(F)" ]
    ]
;;

let key_figures_head = "Min / Max (Overbook)"
let int_to_txt = CCInt.to_string %> txt
let session_path = HttpUtils.Url.Admin.session_path

let some_session_is_followup sessions =
  sessions
  |> CCList.find_opt (fun { Session.follow_up_to; _ } ->
    CCOption.is_some follow_up_to)
  |> CCOption.is_some
;;

module Partials = struct
  open Session

  let table_legend ?(hide_closed = false) language =
    let open Pool_common in
    let to_string = Utils.text_to_string language in
    let open Component.Table in
    let canceled =
      I18n.(to_string Canceled, legend_color_item "bg-red-lighter")
    in
    let closed =
      I18n.(to_string Closed, legend_color_item "bg-green-lighter")
    in
    (if hide_closed then [ canceled ] else [ closed; canceled ]) |> table_legend
  ;;

  let row_classnames { canceled_at; closed_at; follow_up_to; _ } =
    let check opt classname = if opt then Some classname else None in
    [ check (CCOption.is_some canceled_at) "bg-red-lighter"
    ; check (CCOption.is_some closed_at) "bg-green-lighter"
    ; check (CCOption.is_some follow_up_to) "follow-up"
    ]
    |> CCList.filter_map CCFun.id
  ;;

  let row_attrs ({ id; follow_up_to; _ } as session) =
    let id = a_user_data "id" (Id.value id) in
    let classnames = row_classnames session in
    follow_up_to
    |> CCOption.map (fun parent -> a_user_data "parent-id" (Id.value parent))
    |> CCOption.map_or ~default:[ id ] (fun parent -> [ id; parent ])
    |> fun attrs -> a_class classnames :: attrs
  ;;

  let session_row_title language chronological session =
    let date = span [ txt (session |> start_end_with_duration_human) ] in
    match CCOption.is_some session.follow_up_to, chronological with
    | false, true | false, false -> date
    | true, true ->
      div
        ~a:[ a_class [ "flexrow"; "flex-gap-sm" ] ]
        [ date; follow_up_icon language ]
    | true, false -> div ~a:[ a_class [ "inset"; "left" ] ] [ date ]
  ;;

  let session_key_figures
    ({ Session.min_participants; max_participants; overbook; _ } : Session.t)
    =
    let value = ParticipantAmount.value in
    Format.asprintf
      "%i / %i (%i)"
      (min_participants |> value)
      (max_participants |> value)
      (overbook |> value)
  ;;

  let detail_button language experiment_id { id; _ } =
    Format.asprintf
      "/admin/experiments/%s/sessions/%s"
      (Experiment.Id.value experiment_id)
      (Id.value id)
    |> link_as_button
         ~is_text:true
         ~icon:Icon.Eye
         ~control:(language, Message.SessionDetails)
  ;;

  let delete_form
    { Pool_context.language; csrf; _ }
    experiment_id
    ({ id; _ } as session : t)
    =
    if Session.is_deletable session |> CCResult.is_ok
    then
      form
        ~a:
          [ a_method `Post
          ; a_action
              (Format.asprintf
                 "/admin/experiments/%s/sessions/%s/delete"
                 (Experiment.Id.value experiment_id)
                 (Id.value id)
               |> Sihl.Web.externalize_path)
          ; a_user_data
              "confirmable"
              Pool_common.(
                Utils.confirmable_to_string language I18n.DeleteSession)
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
  ;;

  let close_button language experiment_id session =
    if is_closable session |> CCResult.is_ok
    then
      Format.asprintf
        "/admin/experiments/%s/sessions/%s/close"
        (Experiment.Id.value experiment_id)
        (Id.value session.id)
      |> link_as_button
           ~icon:Icon.Create
           ~is_text:true
           ~control:(language, Message.Close (Some Field.Session))
      |> CCOption.return
    else None
  ;;

  let button_dropdown
    ({ Pool_context.language; _ } as context)
    experiment_id
    session
    =
    [ detail_button language experiment_id session |> CCOption.return
    ; delete_form context experiment_id session |> CCOption.return
    ; close_button language experiment_id session
    ]
    |> CCList.filter_map CCFun.id
    |> fun buttons ->
    div
      ~a:[ a_class [ "flexrow" ] ]
      [ Component.ButtonGroup.dropdown ~classnames:[ "push" ] buttons ]
  ;;

  let chronological_toggle language chronological =
    let open Pool_common in
    let to_string = CCFun.(Utils.text_to_string language %> txt) in
    if chronological
    then
      div
        [ p
            [ txt "Sessions marked with "
            ; follow_up_icon language
            ; txt " are follow-up sessions."
            ]
        ; a ~a:[ a_href "?" ] [ to_string I18n.SwitchGrouped ]
        ]
    else
      div
        [ p [ I18n.SessionIndent |> Utils.text_to_string language |> txt ]
        ; a
            ~a:
              [ a_href
                  (Format.asprintf
                     "?%s=true"
                     Message.Field.(show Chronological))
              ]
            [ to_string I18n.SwitchChronological ]
        ]
  ;;
end

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
  (default_email_reminder_lead_time, default_text_msg_reminder_lead_time)
  ?(session : Session.t option)
  ?(follow_up_to : Session.t option)
  ?(duplicate : (Session.t * Session.t list) option)
  locations
  text_messages_enabled
  ~flash_fetcher
  =
  let open CCFun in
  let open Session in
  let open Pool_common in
  let duplicate_parent, _ =
    match duplicate with
    | Some (parent, followups) -> Some parent, Some followups
    | None -> None, None
  in
  let has_assignments =
    session
    |> CCOption.map_or ~default:false (fun s -> s |> Session.has_assignments)
  in
  let default_value_session =
    (* Prefill the form with values if making a duplicate, editing a session or
       creating a follow up to a parent. The importance is 1. duplicate, 2.
       session editing, 3. follow_up_to *)
    CCOption.(duplicate_parent <+> session <+> follow_up_to)
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
  let lead_time_group field get_value encode default_value warning =
    let warning =
      warning
      |> CCOption.map_or
           ~default:(txt "")
           Pool_common.(
             fun hint ->
               hint
               |> Utils.hint_to_string language
               |> txt
               |> CCList.return
               |> Component.Notification.notification language `Warning)
    in
    div
      ~a:[ a_class [ "stack" ] ]
      [ warning
      ; timespan_picker
          language
          field
          ~hints:[ I18n.DefaultReminderLeadTime (default_value |> encode) ]
          ?value:
            (CCOption.bind default_value_session get_value
             |> CCOption.map encode)
          ~flash_fetcher
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
             (* Don't want start date filled out in form if creating follow
                up *)
             if CCOption.is_some follow_up_to
             then None
             else
               default_value_session
               |> CCOption.map (fun (s : t) -> s.start |> Start.value)
           in
           date_time_picker_element
             language
             Message.Field.Start
             ?min_value:
               (follow_up_to
                |> CCOption.map
                     Session.(fun ({ start; _ } : t) -> Start.value start))
             ~required:true
             ~flash_fetcher
             ?value
             ~warn_past:true
             ~read_only:has_assignments)
        ; timespan_picker
            language
            ~required:true
            Message.Field.Duration
            ?value:
              (CCOption.map
                 (fun (s : t) -> s.duration |> Duration.value)
                 default_value_session)
            ~flash_fetcher
            ~read_only:has_assignments
        ; reschedule_hint ()
        ; textarea_element
            language
            Message.Field.InternalDescription
            ~value:
              (value (fun s ->
                 s.internal_description
                 |> CCOption.map_or ~default:"" InternalDescription.value))
            ~flash_fetcher
        ; textarea_element
            language
            Message.Field.PublicDescription
            ~value:
              (value (fun s ->
                 s.public_description
                 |> CCOption.map_or ~default:"" PublicDescription.value))
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
                Reminder.EmailLeadTime.value
                (experiment.Experiment.email_session_reminder_lead_time
                 |> CCOption.value ~default:default_email_reminder_lead_time)
                None
            ; lead_time_group
                Message.Field.TextMessageLeadTime
                (fun (s : t) -> s.text_message_reminder_lead_time)
                Reminder.TextMessageLeadTime.value
                (experiment.Experiment.text_message_session_reminder_lead_time
                 |> CCOption.value ~default:default_text_msg_reminder_lead_time
                )
                (if text_messages_enabled then None else Some I18n.GtxKeyMissing)
            ]
        ]
    ; div
        ~a:[ a_class [ "flexrow" ] ]
        [ submit_element ~classnames:[ "push" ] language submit () ]
    ]
;;

let session_base_information language session =
  let open Session in
  let amount amt = amt |> ParticipantAmount.value |> string_of_int in
  [ Field.Start, session |> Session.start_end_human |> txt
  ; ( Field.Duration
    , session.duration
      |> Duration.value
      |> Pool_common.Utils.Time.formatted_timespan
      |> txt )
  ; ( Field.InternalDescription
    , CCOption.map_or
        ~default:""
        InternalDescription.value
        session.internal_description
      |> Http_utils.add_line_breaks )
  ; ( Field.PublicDescription
    , CCOption.map_or
        ~default:""
        PublicDescription.value
        session.public_description
      |> Http_utils.add_line_breaks )
  ; ( Field.Location
    , Component.Partials.location_to_html language session.Session.location )
  ; Field.MaxParticipants, amount session.max_participants |> txt
  ; Field.MinParticipants, amount session.min_participants |> txt
  ; Field.Overbook, amount session.overbook |> txt
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
  [ p [ txt Pool_common.(Utils.hint_to_string language I18n.RescheduleSession) ]
  ; form
      ~a:
        [ a_class [ "stack" ]
        ; a_method `Post
        ; a_action (action |> Sihl.Web.externalize_path)
        ; a_user_data
            "confirmable"
            (Utils.confirmable_to_string language I18n.RescheduleSession)
        ]
      [ csrf_element csrf ()
      ; div
          ~a:[ a_class [ "grid-col-2" ] ]
          [ date_time_picker_element
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
              ~value:(session.duration |> Duration.value)
              ~flash_fetcher
          ]
      ; div
          ~a:[ a_class [ "flexrow" ] ]
          [ submit_element
              ~classnames:[ "push" ]
              language
              Message.(Reschedule (Some Field.Session))
              ()
          ]
      ]
  ]
  |> Layout.Experiment.(
       create
         context
         (Control Message.(Reschedule (Some Field.Session)))
         experiment)
;;

let data_table
  ({ Pool_context.language; _ } as context)
  experiment
  (sessions, query)
  chronological
  =
  let open Session in
  let session_index_path =
    Format.asprintf
      "/admin/experiments/%s/sessions"
      (experiment.Experiment.id |> Experiment.Id.value)
  in
  let target_id = "session-list" in
  let data_table =
    let url = session_index_path |> Uri.of_string in
    let additional_url_params =
      match chronological with
      | true -> Some [ Message.Field.Chronological, "true" ]
      | false -> None
    in
    Component.DataTable.create_meta
      ?additional_url_params
      ?filter:filterable_by
      url
      query
      language
  in
  let cols =
    let create_session : [ | Html_types.flow5 ] elt =
      link_as_button
        ~style:`Success
        ~icon:Icon.Add
        ~classnames:[ "small"; "nobr" ]
        ~control:(language, Message.(Add (Some Field.Session)))
        (Format.asprintf "%s/create" session_index_path)
    in
    [ `column column_date
    ; `column column_no_assignments
    ; `column column_noshow_count
    ; `column column_participation_count
    ; `custom (txt key_figures_head)
    ; `custom create_session
    ]
  in
  let th_class = [ "w-3"; "w-2"; "w-2"; "w-2"; "w-2"; "w-1" ] in
  let row
    ({ Session.assignment_count
     ; no_show_count
     ; participant_count
     ; closed_at
     ; _
     } as session :
      Session.t)
    =
    let open Partials in
    let row_attrs = Partials.row_attrs session in
    let no_show_count, participant_count =
      match CCOption.is_some closed_at with
      | true ->
        ( no_show_count |> NoShowCount.value |> int_to_txt
        , participant_count |> ParticipantCount.value |> int_to_txt )
      | false -> txt "", txt ""
    in
    [ session_row_title language chronological session
    ; assignment_count |> AssignmentCount.value |> int_to_txt
    ; no_show_count
    ; participant_count
    ; Partials.session_key_figures session |> txt
    ; Partials.button_dropdown context experiment.Experiment.id session
    ]
    |> CCList.map CCFun.(CCList.return %> td)
    |> tr ~a:row_attrs
  in
  DataTable.make
    ~classnames:
      [ "table"; "break-mobile"; "session-list"; "striped"; "align-last-end" ]
    ~target_id
    ~th_class
    ~cols
    ~row
    data_table
    sessions
;;

let index
  ({ Pool_context.language; _ } as context)
  experiment
  sessions
  chronological
  =
  let open Pool_common in
  let hover_script =
    match chronological with
    | false -> txt ""
    | true ->
      let js =
        {js|
          const highlight = "highlighted";
          const initHover = () => {
              const table = document.getElementById("session-list");
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
          };
          initHover();
          document.addEventListener("htmx:afterSettle", () => {
              initHover();
          })
      |js}
      in
      script (Unsafe.data js)
  in
  let chronological_toggle =
    if sessions |> fst |> some_session_is_followup
    then Partials.chronological_toggle language chronological
    else txt ""
  in
  div
    ~a:[ a_class [ "stack" ] ]
    [ chronological_toggle
    ; Partials.table_legend language
    ; data_table context experiment sessions chronological
    ; hover_script
    ]
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
  default_leadtime_settings
  locations
  text_messages_enabled
  flash_fetcher
  =
  session_form
    csrf
    language
    experiment
    default_leadtime_settings
    locations
    text_messages_enabled
    ~flash_fetcher
  |> CCList.return
  |> Layout.Experiment.(
       create context (Control Message.(Create (Some Field.Session))) experiment)
;;

let duplicate_form ?parent_session language session followups form_id =
  let open Session in
  let open Pool_common in
  let remove_button =
    let classes style = [ style; "has-icon" ] in
    let make_button style attributes =
      button
        ~a:([ a_class (classes style); a_button_type `Button ] @ attributes)
        [ Component.Icon.(to_html TrashOutline) ]
    in
    if form_id = 0
    then make_button "disabled" [ a_disabled () ]
    else make_button "error" [ a_user_data "remove-group" "" ]
  in
  let min_date ({ start; _ } : t) =
    Start.value start |> Component.Input.flatpickr_min
  in
  let input_name { id; _ } =
    Format.asprintf "%s[%i]" (Session.Id.value id) form_id
  in
  let input ?min_input_el ?value session =
    let name = input_name session in
    let label_txt =
      Format.asprintf
        "Session from %s"
        (Session.start_end_with_duration_human session)
      |> txt
    in
    let attrs =
      let min_date = parent_session |> CCOption.map min_date in
      let min_input_el =
        min_input_el
        |> CCOption.map (fun min_input ->
          a_user_data "min-input-element" (input_name min_input))
      in
      [ min_date; min_input_el ] |> CCList.filter_map CCFun.id
    in
    div
      ~a:[ a_class [ "form-group" ] ]
      [ label ~a:[ a_label_for name ] [ label_txt ]
      ; input
          ~a:
            ([ a_id name
             ; a_name name
             ; a_class [ "datepicker" ]
             ; a_value (CCOption.value ~default:"" value)
             ; a_user_data
                 "warn-past"
                 (Utils.hint_to_string language I18n.SelectedDateIsPast)
             ; a_required ()
             ]
             @ attrs)
          ()
      ; span ~a:[ a_class [ "help"; "datepicker-msg"; "error-message" ] ] []
      ]
  in
  let wrap title html =
    div
      ~a:[ a_class [ "flexcolumn" ] ]
      [ h4
          [ Utils.field_to_string language title
            |> CCString.capitalize_ascii
            |> txt
          ]
      ; html
      ]
  in
  let main = div [ input session ] |> wrap Message.Field.MainSession in
  let followups =
    if CCList.is_empty followups
    then div [ txt "" ]
    else
      followups
      |> CCList.map (input ~min_input_el:session)
      |> div ~a:[ a_class [ "stack" ] ]
      |> wrap Message.Field.FollowUpSession
  in
  div
    ~a:
      [ a_class [ "border-bottom"; "inset"; "vertical"; "flexrow"; "flex-gap" ]
      ; a_user_data "duplicate-form" (CCInt.to_string form_id)
      ]
    [ div ~a:[ a_class [ "grid-col-2"; "grow" ] ] [ main; followups ]
    ; div ~a:[ a_class [ "flexcolumn"; "justify-end" ] ] [ remove_button ]
    ]
;;

let duplicate
  ({ Pool_context.language; csrf; _ } as context)
  ?parent_session
  experiment
  session
  followups
  =
  let open Session in
  let session_info =
    let session_link session =
      span
        ~a:[ a_class [ "has-icon" ] ]
        [ txt (Session.start_end_with_duration_human session)
        ; a
            ~a:
              [ a_href
                  (session_path experiment.Experiment.id session.id
                   |> Sihl.Web.externalize_path)
              ; a_target "_blank"
              ]
            [ Icon.(to_html OpenOutline) ]
        ]
    in
    let main = session_link session in
    let wrap ~inset =
      let classname =
        [ "flexcolumn"; "stack-sm" ] @ if inset then [ "inset"; "left" ] else []
      in
      div ~a:[ a_class classname ]
    in
    let session_list =
      match followups with
      | [] -> wrap ~inset:false [ main ]
      | followups ->
        let items = CCList.map session_link followups in
        items
        |> wrap ~inset:true
        |> fun followups -> [ main; followups ] |> wrap ~inset:false
    in
    div
      ~a:[ a_class [ "gap-lg" ] ]
      [ p
          [ txt
              Pool_common.(
                Utils.hint_to_string language I18n.DuplicateSessionList)
          ]
      ; session_list
      ]
  in
  let hint =
    Pool_common.(Utils.hint_to_string language I18n.DuplicateSession)
    |> txt
    |> CCList.return
    |> Component.Notification.notification language `Warning
  in
  let subform_wrapper = "session-duplication-subforms" in
  let session_path = session_path experiment.Experiment.id session.id in
  let add_subform_button =
    button
      ~a:
        Htmx.
          [ a_class [ "success"; "has-icon" ]
          ; hx_trigger "click"
          ; hx_get
              (Format.asprintf "%s/duplicate/form" session_path
               |> Sihl.Web.externalize_path)
          ; hx_target ("#" ^ subform_wrapper)
          ; hx_swap "beforeend"
          ]
      [ Icon.(to_html Add) ]
  in
  let submit_button =
    button
      ~a:
        Htmx.
          [ a_class [ "primary" ]
          ; hx_trigger "click"
          ; hx_swap "none"
          ; hx_post
              (Format.sprintf "%s/duplicate" session_path
               |> Sihl.Web.externalize_path)
          ]
      [ txt
          Pool_common.(
            Utils.control_to_string
              language
              (Message.Create (Some Field.Sessions)))
      ]
  in
  let form =
    form
      ~a:[ a_id "session-duplication-form" ]
      [ Component.Input.csrf_element csrf ()
      ; div
          ~a:[ a_id subform_wrapper ]
          [ duplicate_form ?parent_session language session followups 0 ]
      ; div
          ~a:[ a_class [ "flexrow"; "gap" ] ]
          [ div
              ~a:[ a_class [ "flexrow"; "flex-gap"; "push" ] ]
              [ add_subform_button; submit_button ]
          ]
      ]
  in
  [ div ~a:[ a_class [ "stack" ] ] [ hint; session_info; form ] ]
  |> Layout.Experiment.(
       create
         context
         (Control Message.(Duplicate (Some Field.Session)))
         experiment)
;;

let detail
  ?access_contact_profiles
  ?view_contact_name
  ?view_contact_info
  (Pool_context.{ language; csrf; _ } as context)
  experiment
  (session : Session.t)
  participation_tags
  sys_languages
  session_reminder_templates
  text_messages_enabled
  (assignments, query)
  =
  let open Pool_common in
  let open Session in
  let experiment_id = experiment.Experiment.id in
  let session_path = session_path experiment_id session.id in
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
        (Format.asprintf "%s/%s" session_path url)
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
          Format.asprintf "%s/resend-reminders" session_path
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
                  Channel.(filtered_channels text_messages_enabled)
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
      let no_show_count, participant_count =
        (fun (no_show_count, participant_count) ->
          ( (Field.NoShowCount, no_show_count)
          , (Field.Participated, participant_count) ))
        @@
        match CCOption.is_some session.closed_at with
        | true ->
          ( session.no_show_count |> NoShowCount.value |> int_to_txt
          , session.participant_count |> ParticipantCount.value |> int_to_txt )
        | false -> txt "", txt ""
      in
      let rows =
        session_base_information language session
        @ [ ( Field.AssignmentCount
            , session.assignment_count |> AssignmentCount.value |> int_to_txt )
          ; no_show_count
          ; participant_count
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
        rows @ time_stamps @ ([ canceled; closed ] |> CCList.filter_map CCFun.id)
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
          Format.asprintf "%s/%s/duplicate" base (Id.value session.id)
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
  let message_templates_html label list =
    let open Message_template in
    let build_path append template =
      Format.asprintf
        "%s/%s"
        session_path
        (Message_template.prefixed_template_url ~append template)
    in
    let edit_path = build_path "edit" in
    let delete_path = build_path "delete", csrf in
    let buttons =
      if CCList.is_empty (Message_template.filter_languages sys_languages list)
      then None
      else (
        let path =
          Format.asprintf "%s/%s" session_path Label.(prefixed_human_url label)
        in
        Some (Button.add label path))
    in
    div
      [ h2 ~a:[ a_class [ "heading-2" ] ] [ txt (Label.to_human label) ]
      ; Page_admin_message_template.(
          experiment_help ~entity:(Session session.id) language [ label ])
      ; div
          ~a:[ a_class [ "gap" ] ]
          [ Page_admin_message_template.table
              ?buttons
              ~delete_path
              language
              list
              edit_path
          ]
      ]
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
    let open Page_admin_assignments in
    let swap_session_modal_id = swap_session_modal_id session in
    let direct_message_modal_id = direct_message_modal_id session in
    let legend = Partials.table_legend language in
    let modal id =
      div ~a:[ a_id id; a_class [ "fullscreen-overlay"; "modal" ] ] []
    in
    let assignment_list =
      data_table
        ?access_contact_profiles
        ?view_contact_name
        ?view_contact_info
        Session
        context
        experiment
        session
        text_messages_enabled
        (assignments, query)
    in
    let swap_session_modal_js =
      Format.asprintf
        {js|
        const modalId = "%s";
      %s
      document.addEventListener("htmx:afterSwap", (e) => {
        if (e.srcElement.id != modalId) {
          return
        }
        const modal = e.detail.elt;
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
        swap_session_modal_id
        (Component.Modal.js_modal_add_spinner swap_session_modal_id)
    in
    div
      ~a:[ a_class [ "stack" ] ]
      [ div
          ~a:
            [ a_class
                [ "flexrow"
                ; "flex-gap"
                ; "justify-between"
                ; "flexcolumn-mobile"
                ]
            ]
          [ div
              [ h2
                  ~a:[ a_class [ "heading-2" ] ]
                  [ txt (Utils.nav_link_to_string language I18n.Assignments) ]
              ]
          ; button
              ~a:
                [ a_class [ "btn"; "primary"; "has-icon"; "small" ]
                ; a_user_data "print" "assignments"
                ]
              [ Icon.(to_html PrintOutline)
              ; txt
                  Pool_common.(
                    Utils.control_to_string
                      language
                      Message.(Print (Some Field.Assignments)))
              ]
          ]
      ; legend
      ; modal swap_session_modal_id
      ; modal direct_message_modal_id
      ; assignment_list
      ; script (Unsafe.data swap_session_modal_js)
      ]
  in
  let edit_button =
    link_as_button
      ~icon:Icon.Create
      ~classnames:[ "small" ]
      ~control:(language, Message.(Edit (Some Field.Session)))
      (Format.asprintf "%s/edit" session_path)
  in
  div
    ~a:[ a_class [ "stack-lg" ] ]
    [ session_overview
    ; tags_html
    ; message_templates_html
        Message_template.Label.SessionReminder
        session_reminder_templates
    ; assignments_html
    ]
  |> CCList.return
  |> Layout.Experiment.(
       create
         ~buttons:edit_button
         context
         (I18n (session_title session))
         experiment)
;;

let print
  ?view_contact_name
  ?view_contact_info
  (Pool_context.{ language; _ } as context)
  experiment
  (session : Session.t)
  assignments
  =
  let assignment_list =
    Page_admin_assignments.(
      Partials.overview_list
        ?view_contact_name
        ?view_contact_info
        ~is_print:true
        Session
        context
        experiment
        session
        assignments
        false)
  in
  let title =
    Pool_common.(Utils.text_to_string language (session_title session))
  in
  [ div ~a:[ a_class [ "safety-margin" ] ] [ h1 [ txt title ]; assignment_list ]
  ]
  |> Layout.Print.create ~document_title:title
;;

let edit
  ({ Pool_context.language; csrf; _ } as context)
  experiment
  default_leadtime_settings
  (session : Session.t)
  locations
  (current_tags, available_tags, experiment_tags)
  text_messages_enabled
  flash_fetcher
  =
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
          default_leadtime_settings
          ~session
          locations
          text_messages_enabled
          ~flash_fetcher
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
            [ Utils.hint_to_string language I18n.ParticipationTagsHint |> txt ]
      ; div
          ~a:[ a_class [ "switcher-lg"; "flex-gap" ] ]
          [ Tag.add_tags_form
              context
              ~existing:current_tags
              available_tags
              (tags_action "assign")
          ; div
              ~a:[ a_class [ "flexcolumn"; "flex-gap" ] ]
              [ Component.Tag.tag_form
                  ~label:Pool_common.I18n.SelectedTags
                  language
                  (remove_action, csrf)
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
  div ~a:[ a_class [ "stack-lg" ] ] [ form; tags_html ]
  |> CCList.return
  |> Layout.Experiment.(
       create context (Control Message.(Edit (Some Field.Session))) experiment)
;;

let follow_up
  ({ Pool_context.language; csrf; _ } as context)
  ?duplicate
  experiment
  default_leadtime_settings
  (parent_session : Session.t)
  locations
  text_messages_enabled
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
        default_leadtime_settings
        ?duplicate
        ~follow_up_to:parent_session
        locations
        text_messages_enabled
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
        ~a:[ a_class [ "session-close-checkboxes" ] ]
        [ div [ strong [ CCInt.to_string num_participations |> txt ] ]
        ; div [ strong [ CCInt.to_string num_no_shows |> txt ] ]
        ]
    ]
;;

let close_assignment_htmx_form
  { Pool_context.language; csrf; _ }
  (experiment : Experiment.t)
  ?(updated_fields = [])
  session
  ?counters
  ({ Assignment.id; no_show; participated; external_data_id; _ } as assignment)
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
      then [ a_class [ "is-valid" ] ]
      else []
    in
    div
      ~a:[ a_class [ "form-group" ] ]
      [ div
          ~a:[ a_class [ "flexrow"; "justify-center" ] ]
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
  let action =
    Format.asprintf "%s/assignments/%s/close" session_path (Id.value id)
    |> Sihl.Web.externalize_path
  in
  let external_data_field =
    match
      Experiment.(
        experiment |> external_data_required |> ExternalDataRequired.value)
    with
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
        then [ a_class [ "is-invalid" ] ]
        else if CCList.mem
                  ~eq:Pool_common.Message.Field.equal
                  field
                  updated_fields
        then [ a_class [ "is-valid" ] ]
        else []
      in
      div
        ~a:[ a_class [ "form-group"; "grow" ] ]
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
      CCList.map error_to_item errors
      |> ul ~a:[ a_class [ "color-red"; "flexrow" ] ])
  in
  form
    ~a:
      [ a_user_data "hx-post" action
      ; a_user_data "hx-trigger" "change"
      ; a_user_data "hx-swap" "outerHTML"
      ; a_user_data "assignment" (Id.value id)
      ; a_class [ "flexcolumn"; "stack-sm"; "w-4" ]
      ]
    [ csrf_element csrf ()
    ; div
        ~a:[ a_class [ "flexrow"; "flex-gap-sm" ] ]
        [ external_data_field
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
  ({ Pool_context.language; csrf; user; _ } as context)
  experiment
  (session : Session.t)
  assignments
  custom_fields
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
      let identity_width = "w-3" in
      let custom_data_width = "w-5" in
      let thead =
        let form_header =
          div
            ~a:[ a_class [ "flexrow"; "w-4" ] ]
            [ div
                ~a:[ a_class [ "session-close-checkboxes" ] ]
                [ div [ strong [ txt "P" ] ]; div [ strong [ txt "NS" ] ] ]
            ]
        in
        [ div ~a:[ a_class [ identity_width ] ] []
        ; custom_fields
          |> CCList.map (fun field ->
            div [ txt (Custom_field.name_value language field) ])
          |> div ~a:[ a_class [ custom_data_width; "custom-data" ] ]
        ]
        @ [ form_header ]
      in
      CCList.map
        (fun ({ Assignment.id; contact; _ } as assignment) ->
          let custom_data = assignment.Assignment.custom_fields in
          let custom_field_cells =
            let open Custom_field in
            custom_data
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
            |> div ~a:[ a_class [ custom_data_width; "custom-data" ] ]
          in
          let identity =
            Component.UserStatus.Contact.identity
              view_contact_name
              contact
              (Assignment.Id.to_common id)
          in
          [ div ~a:[ a_class [ identity_width ] ] [ strong [ txt identity ] ]
          ; custom_field_cells
          ; close_assignment_htmx_form context experiment session assignment
          ])
        assignments
      |> CCList.map (div ~a:[ a_class [ "inset-sm"; "flexrow" ] ])
      |> fun rows ->
      let table =
        div
          ~a:[ a_class [ "session-close-table" ] ]
          [ div
              ~a:[ a_class [ "flexrow"; "inset-sm"; "session-close-header" ] ]
              thead
          ; div ~a:[ a_class [ "striped" ] ] rows
          ; session_counters language counters
          ]
      in
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
      div [ table; script (Unsafe.data scripts) ]
  in
  let submit_session_close =
    form
      ~a:
        [ a_method `Post
        ; a_class [ "stack" ]
        ; a_action
            (Format.asprintf "%s/close" session_path
             |> Sihl.Web.externalize_path)
        ; a_user_data
            "confirmable"
            Pool_common.(Utils.confirmable_to_string language I18n.CloseSession)
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
      ; Page_admin_assignments.Partials.table_legend ~hide_deleted:true language
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
               ~hints:[ I18n.SessionCancelMessage ]
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
  form_context
  flash_fetcher
  =
  let open Message_template in
  let open Pool_common in
  let control_to_title control =
    Layout.Experiment.Text
      (Format.asprintf
         "%s %s"
         (control |> Utils.control_to_string language)
         (label |> Label.to_human |> CCString.lowercase_ascii))
  in
  let control =
    match form_context with
    | `Create _ -> Message.(Create None)
    | `Update _ -> Message.(Edit None)
  in
  let action =
    let path =
      Format.asprintf
        "/admin/experiments/%s/sessions/%s/%s"
        Experiment.(Id.value experiment.Experiment.id)
        Session.(Id.value session.Session.id)
    in
    match form_context with
    | `Create t -> path (Label.prefixed_human_url t.label)
    | `Update t -> path (prefixed_template_url t)
  in
  let text_elements =
    Component.MessageTextElements.message_template_help
      ~experiment
      ~session
      language
      tenant
      label
  in
  let open Page_admin_message_template in
  template_form
    context
    ~entity:(Session session.Session.id)
    ~text_elements
    ?languages
    form_context
    tenant.Pool_tenant.text_messages_enabled
    action
    flash_fetcher
  |> CCList.return
  |> Layout.Experiment.create context (control_to_title control) experiment
;;
