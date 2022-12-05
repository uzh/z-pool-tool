open Tyxml.Html
open Component
open Input
module Message = Pool_common.Message

let session_title (s : Session.t) =
  Pool_common.I18n.SessionDetailTitle (s.Session.start |> Session.Start.value)
;;

let location_select options selected ?(attributes = []) () =
  let open Pool_location in
  let name = Message.Field.(show Location) in
  div
    ~a:[ a_class [ "form-group" ] ]
    [ label [ txt (name |> CCString.capitalize_ascii) ]
    ; div
        ~a:[ a_class [ "select" ] ]
        [ select
            ~a:([ a_name name ] @ attributes)
            (CCList.map
               (fun l ->
                 let is_selected =
                   selected
                   |> CCOption.map (fun selected ->
                        if Pool_location.equal selected l
                        then [ a_selected () ]
                        else [])
                   |> CCOption.value ~default:[]
                 in
                 option
                   ~a:
                     ([ a_value (l.id |> Pool_location.Id.value) ] @ is_selected)
                   (txt (l.name |> Pool_location.Name.value)))
               options)
        ]
    ]
;;

let session_form
  csrf
  language
  (experiment : Experiment.t)
  ?(session : Session.t option)
  ?(follow_up_to : Session.t option)
  locations
  sys_languages
  ~flash_fetcher
  =
  let open CCFun in
  let open Session in
  let default_value_session = CCOption.(session <+> follow_up_to) in
  let has_assignments =
    default_value_session
    |> CCOption.map_or ~default:false (fun s -> s |> Session.has_assignments)
  in
  let reschedule_hint () =
    match session, has_assignments with
    | Some session, true ->
      let action =
        let open Pool_common.Id in
        Format.asprintf
          "/admin/experiments/%s/sessions/%s/reschedule"
          (value experiment.Experiment.id)
          (value session.Session.id)
        |> Sihl.Web.externalize_path
      in
      p
        [ txt "There are assignments for this session. Please use the "
        ; a
            ~a:[ a_href action ]
            [ txt "form provided to reschedule a session." ]
        ]
    | _ -> txt ""
  in
  let value = CCFun.flip (CCOption.map_or ~default:"") default_value_session in
  let amount fnc = value (fnc %> ParticipantAmount.value %> CCInt.to_string) in
  let lead_time_value time =
    time
    |> CCOption.map_or
         ~default:""
         Pool_common.(Reminder.LeadTime.value %> Utils.Time.timespan_spanpicker)
  in
  let to_default_value html =
    div ~a:[ a_class [ "gap"; "inset-sm"; "border-left" ] ] [ html ]
  in
  let action, submit =
    let open Pool_common in
    let base =
      Format.asprintf
        "/admin/experiments/%s/sessions"
        (Pool_common.Id.value experiment.Experiment.id)
    in
    match session, follow_up_to with
    | None, None -> base, Message.(Create (Some Field.Session))
    | None, Some follow_up_to ->
      ( Format.asprintf
          "%s/%s/follow-up"
          base
          (follow_up_to.Session.id |> Pool_common.Id.value)
      , Message.(Create (Some Field.FollowUpSession)) )
    | Some session, _ ->
      ( Format.asprintf "%s/%s" base (session.id |> Pool_common.Id.value)
      , Message.(Update (Some Field.Session)) )
  in
  form
    ~a:
      [ a_class [ "stack" ]
      ; a_method `Post
      ; a_action (action |> Sihl.Web.externalize_path)
      ]
    [ csrf_element csrf ()
    ; div
        ~a:[ a_class [ "grid-col-2" ] ]
        [ flatpicker_element
            language
            `Datetime_local
            Pool_common.Message.Field.Start
            ~required:true
            ~flash_fetcher
            ~value:(value (fun s -> s.start |> Start.value |> Ptime.to_rfc3339))
            ~warn_past:true
            ~additional_attributes:
              (if has_assignments then [ a_disabled () ] else [])
        ; flatpicker_element
            language
            ~required:true
            `Time
            Pool_common.Message.Field.Duration
            ~help:Pool_common.I18n.TimeSpanPickerHint
            ~value:
              (value (fun s ->
                 s.duration
                 |> Duration.value
                 |> Pool_common.Utils.Time.timespan_spanpicker))
            ~flash_fetcher
            ~additional_attributes:
              (if has_assignments then [ a_disabled () ] else [])
        ; reschedule_hint ()
        ; textarea_element
            language
            Pool_common.Message.Field.Description
            ~value:
              (value (fun s ->
                 s.description |> CCOption.map_or ~default:"" Description.value))
            ~flash_fetcher
        ; location_select locations None ()
        ; input_element
            language
            `Number
            Pool_common.Message.Field.MaxParticipants
            ~required:true
            ~value:(amount (fun s -> s.max_participants))
            ~flash_fetcher
        ; input_element
            language
            `Number
            Pool_common.Message.Field.MinParticipants
            ~required:true
            ~value:(amount (fun s -> s.min_participants))
            ~flash_fetcher
        ; input_element
            language
            `Number
            Pool_common.Message.Field.Overbook
            ~required:true
            ~value:(amount (fun s -> s.overbook))
            ~flash_fetcher
        ]
    ; div
        ~a:[ a_class [ "gap-lg" ] ]
        [ h3
            ~a:[ a_class [ "heading-3" ] ]
            [ txt Pool_common.(Utils.text_to_string language I18n.Reminder) ]
        ; div
            ~a:[ a_class [ "grid-col-2" ] ]
            [ div
                [ flatpicker_element
                    language
                    `Time
                    Pool_common.Message.Field.LeadTime
                    ~help:Pool_common.I18n.TimeSpanPickerHint
                    ~value:
                      (value (fun s -> s.reminder_lead_time |> lead_time_value))
                    ~flash_fetcher
                ; experiment.Experiment.session_reminder_lead_time
                  |> CCOption.map_or ~default:(txt "") (fun leadtime ->
                       Pool_common.(
                         Utils.text_to_string
                           language
                           (I18n.SessionReminderDefaultLeadTime
                              (leadtime |> Reminder.LeadTime.value)))
                       |> txt
                       |> to_default_value)
                ]
            ; div
                ~a:[ a_class [ "full-width" ] ]
                [ MessageTextElements.session_reminder_help
                    language
                    sys_languages
                    ?session:default_value_session
                    ()
                ]
            ; input_element
                language
                `Text
                Pool_common.Message.Field.ReminderSubject
                ~value:
                  (value (fun s ->
                     s.reminder_subject
                     |> CCOption.map_or
                          ~default:""
                          Pool_common.Reminder.Subject.value))
                ~flash_fetcher
            ; experiment.Experiment.session_reminder_subject
              |> CCOption.map_or ~default:(txt "") (fun text ->
                   Pool_common.(
                     Utils.text_to_string
                       language
                       (I18n.SessionReminderDefaultSubject
                          (text |> Reminder.Subject.value)))
                   |> Http_utils.add_line_breaks
                   |> to_default_value)
            ; textarea_element
                language
                Pool_common.Message.Field.ReminderText
                ~value:
                  (value (fun s ->
                     s.reminder_text
                     |> CCOption.map_or
                          ~default:""
                          Pool_common.Reminder.Text.value))
                ~flash_fetcher
            ; experiment.Experiment.session_reminder_text
              |> CCOption.map_or ~default:(txt "") (fun text ->
                   Pool_common.(
                     Utils.text_to_string
                       language
                       (I18n.SessionReminderDefaultText
                          (text |> Reminder.Text.value)))
                   |> Http_utils.add_line_breaks
                   |> to_default_value)
            ]
        ]
    ; div
        ~a:[ a_class [ "flexrow" ] ]
        [ submit_element ~classnames:[ "push" ] language submit () ]
    ]
;;

let reschedule_session
  Pool_context.{ csrf; language; _ }
  experiment
  (session : Session.t)
  flash_fetcher
  =
  let open Session in
  let action =
    let open Pool_common.Id in
    Format.asprintf
      "/admin/experiments/%s/sessions/%s/reschedule"
      (value experiment.Experiment.id)
      (value session.Session.id)
  in
  form
    ~a:
      [ a_class [ "stack" ]
      ; a_method `Post
      ; a_action (action |> Sihl.Web.externalize_path)
      ]
    [ csrf_element csrf ()
    ; flatpicker_element
        language
        `Datetime_local
        Pool_common.Message.Field.Start
        ~required:true
        ~flash_fetcher
        ~value:(session.start |> Start.value |> Ptime.to_rfc3339)
        ~disable_past:true
    ; flatpicker_element
        language
        ~required:true
        `Time
        Pool_common.Message.Field.Duration
        ~help:Pool_common.I18n.TimeSpanPickerHint
        ~value:
          (session.duration
          |> Duration.value
          |> Pool_common.Utils.Time.timespan_spanpicker)
        ~flash_fetcher
    ; div
        ~a:[ a_class [ "flexrow" ] ]
        [ submit_element
            ~classnames:[ "push" ]
            language
            Pool_common.Message.(Reschedule (Some Field.Session))
            ()
        ]
    ]
  |> Page_admin_experiments.experiment_layout
       language
       (Page_admin_experiments.Control
          Pool_common.Message.(Reschedule (Some Field.Session)))
       experiment
;;

let index
  Pool_context.{ language; csrf; _ }
  experiment
  grouped_sessions
  chronological
  =
  let experiment_id = experiment.Experiment.id in
  let add_session_btn =
    link_as_button
      ~style:`Success
      ~icon:`Add
      ~classnames:[ "small" ]
      ~control:(language, Message.(Add (Some Field.Session)))
      (Format.asprintf
         "/admin/experiments/%s/sessions/create"
         (experiment_id |> Pool_common.Id.value))
  in
  let rows =
    CCList.flat_map
      (fun (parent, follow_ups) ->
        let open Session in
        let session_row session =
          let cancel_form =
            match CCOption.is_some session.Session.canceled_at with
            | true ->
              submit_element
                ~submit_type:`Disabled
                language
                Message.(Cancel None)
                ()
            | false ->
              form
                ~a:
                  [ a_method `Post
                  ; a_action
                      (Format.asprintf
                         "/admin/experiments/%s/sessions/%s/cancel"
                         (Pool_common.Id.value experiment_id)
                         (Pool_common.Id.value session.id)
                      |> Sihl.Web.externalize_path)
                  ; a_user_data
                      "confirmable"
                      Pool_common.(
                        Utils.confirmable_to_string language I18n.CancelSession)
                  ]
                [ csrf_element csrf ()
                ; submit_element language Message.(Cancel None) ()
                ]
          in
          let delete_form =
            if session.Session.assignment_count
               |> Session.AssignmentCount.value
               == 0
            then
              form
                ~a:
                  [ a_method `Post
                  ; a_action
                      (Format.asprintf
                         "/admin/experiments/%s/sessions/%s/delete"
                         (Pool_common.Id.value experiment_id)
                         (Pool_common.Id.value session.id)
                      |> Sihl.Web.externalize_path)
                  ; a_user_data
                      "confirmable"
                      Pool_common.(
                        Utils.confirmable_to_string language I18n.DeleteSession)
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
          let attrs =
            if CCOption.is_some session.follow_up_to && not chronological
            then [ a_class [ "inset"; "left" ] ]
            else []
          in
          [ div ~a:attrs [ txt (session |> Session.session_date_to_human) ]
          ; txt
              (CCInt.to_string
                 (session.Session.assignment_count
                 |> Session.AssignmentCount.value))
          ; session.Session.canceled_at
            |> CCOption.map_or ~default:"" (fun t ->
                 Pool_common.Utils.Time.formatted_date_time t)
            |> txt
          ; div
              ~a:[ a_class [ "flexrow"; "flex-gap"; "justify-end" ] ]
              [ Format.asprintf
                  "/admin/experiments/%s/sessions/%s"
                  (Pool_common.Id.value experiment_id)
                  (Pool_common.Id.value session.id)
                |> edit_link
              ; cancel_form
              ; delete_form
              ]
          ]
        in
        session_row parent :: CCList.map session_row follow_ups)
      grouped_sessions
  in
  let thead =
    Pool_common.Message.(
      [ Field.Date; Field.AssignmentCount; Field.CanceledAt ]
      |> Table.fields_to_txt language)
    @ [ add_session_btn ]
  in
  let html =
    div
      ~a:[ a_class [ "stack" ] ]
      [ p
          [ Pool_common.I18n.SessionIndent
            |> Pool_common.Utils.text_to_string language
            |> txt
          ]
      ; a
          ~a:
            [ a_href
                (Format.asprintf
                   "/admin/experiments/%s/sessions%s"
                   (Pool_common.Id.value experiment_id)
                   (if chronological then "" else "?chronological=true")
                |> Sihl.Web.externalize_path)
            ]
          [ p
              [ (if chronological
                then Pool_common.I18n.SwitchGrouped
                else Pool_common.I18n.SwitchChronological)
                |> Pool_common.Utils.text_to_string language
                |> txt
              ]
          ]
        (* TODO [aerben] allow tables to be sorted generally? *)
      ; Table.horizontal_table `Striped ~align_last_end:true ~thead rows
      ]
  in
  Page_admin_experiments.experiment_layout
    ~hint:Pool_common.I18n.ExperimentSessions
    language
    (Page_admin_experiments.NavLink Pool_common.I18n.Sessions)
    experiment
    ~active:Pool_common.I18n.Sessions
    html
;;

let new_form
  Pool_context.{ language; csrf; _ }
  experiment
  locations
  sys_languages
  flash_fetcher
  =
  Page_admin_experiments.experiment_layout
    language
    (Page_admin_experiments.Control
       Pool_common.Message.(Create (Some Field.Session)))
    experiment
    (session_form
       csrf
       language
       experiment
       locations
       sys_languages
       ~flash_fetcher)
;;

let detail
  (Pool_context.{ language; _ } as context)
  experiment
  (session : Session.t)
  assignments
  =
  let open Session in
  let session_link (show, url, control) =
    match show with
    | false -> None
    | true ->
      link_as_button
        ~control:(language, control)
        ~classnames:[ "small" ]
        (Format.asprintf
           "/admin/experiments/%s/sessions/%s/%s"
           (Pool_common.Id.value experiment.Experiment.id)
           (Pool_common.Id.value session.id)
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
                         (Pool_common.Id.value experiment.Experiment.id)
                         (Pool_common.Id.value follow_up_to)
                      |> Sihl.Web.externalize_path)
                  ]
                [ Message.Show
                  |> Pool_common.Utils.control_to_string language
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
            |> Pool_common.Utils.Time.formatted_date_time
            |> txt )
        ; ( Field.Duration
          , session.duration
            |> Duration.value
            |> Pool_common.Utils.Time.formatted_timespan
            |> txt )
        ; ( Field.Description
          , CCOption.map_or ~default:"" Description.value session.description
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
               ( Field.CanceledAt
               , Pool_common.Utils.Time.formatted_date_time c |> txt ))
        in
        let closed =
          session.closed_at
          |> CCOption.map (fun c ->
               ( Field.ClosedAt
               , Pool_common.Utils.Time.formatted_date_time c |> txt ))
        in
        rows @ ([ canceled; closed ] |> CCList.filter_map CCFun.id)
      in
      Table.vertical_table `Striped language ~align_top:true
      @@ CCOption.map_or ~default:rows (CCList.cons' rows) parent
    in
    let links =
      Message.
        [ ( CCOption.is_none session.follow_up_to
          , "follow-up"
          , Create (Some Field.FollowUpSession) )
        ; ( session.assignment_count |> AssignmentCount.value > 0
            && CCOption.is_none session.closed_at
          , "reschedule"
          , Reschedule (Some Field.Session) )
        ; ( CCOption.is_none session.closed_at
            && Ptime.is_earlier
                 (session.start |> Start.value)
                 ~than:Ptime_clock.(now ())
          , "close"
          , Close (Some Field.Session) )
        ]
      |> CCList.filter_map session_link
    in
    div
      ~a:[ a_class [ "stack" ] ]
      [ table; p ~a:[ a_class [ "flexrow"; "flex-gap" ] ] links ]
  in
  let assignments_html =
    let assignment_list =
      Page_admin_assignments.Partials.overview_list
        context
        experiment.Experiment.id
        assignments
    in
    div
      [ h2
          ~a:[ a_class [ "heading-2" ] ]
          [ txt Pool_common.(Utils.nav_link_to_string language I18n.Assignments)
          ]
      ; assignment_list
      ]
  in
  let edit_button =
    link_as_button
      ~icon:`CreateOutline
      ~classnames:[ "small" ]
      ~control:(language, Message.(Edit (Some Field.Session)))
      (Format.asprintf
         "/admin/experiments/%s/sessions/%s/edit"
         (Pool_common.Id.value experiment.Experiment.id)
         (Pool_common.Id.value session.id))
  in
  let html =
    div ~a:[ a_class [ "stack-lg" ] ] [ session_overview; assignments_html ]
  in
  Page_admin_experiments.experiment_layout
    ~buttons:edit_button
    language
    (Page_admin_experiments.I18n (session_title session))
    experiment
    html
;;

let edit
  Pool_context.{ language; csrf; _ }
  experiment
  (session : Session.t)
  locations
  sys_languages
  flash_fetcher
  =
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
        ~session
        locations
        sys_languages
        ~flash_fetcher
    ]
  |> Page_admin_experiments.experiment_layout
       language
       (Page_admin_experiments.Control
          Pool_common.Message.(Edit (Some Field.Session)))
       experiment
;;

let follow_up
  Pool_context.{ language; csrf; _ }
  experiment
  (parent_session : Session.t)
  locations
  sys_languages
  flash_fetcher
  =
  div
    [ p
        [ txt
            Pool_common.Utils.(
              parent_session
              |> session_title
              |> text_to_string language
              |> CCFormat.asprintf
                   "%s %s"
                   (Pool_common.I18n.FollowUpSessionFor
                   |> text_to_string language))
        ]
    ; session_form
        csrf
        language
        experiment
        ~follow_up_to:parent_session
        locations
        sys_languages
        ~flash_fetcher
    ]
  |> Page_admin_experiments.experiment_layout
       language
       (Page_admin_experiments.Control
          Pool_common.Message.(Create (Some Field.FollowUpSession)))
       experiment
;;

let close
  Pool_context.{ language; csrf; _ }
  experiment
  (session : Session.t)
  assignments
  =
  let open Pool_common in
  let control = Message.(Close (Some Field.Session)) in
  let field_to_string f =
    Utils.field_to_string language f |> CCString.capitalize_ascii
  in
  let form =
    let checkbox_element ?(disabled = false) contact field =
      let id =
        Format.asprintf
          "%s_%s"
          (Contact.id contact |> Id.value)
          Message.Field.(show field)
      in
      let disabled = if disabled then [ a_disabled () ] else [] in
      div
        [ input
            ~a:
              ([ a_input_type `Checkbox
               ; a_name Message.Field.(array_key field)
               ; a_id id
               ; a_value (contact |> Contact.id |> Id.value)
               ]
              @ disabled)
            ()
        ; label
            ~a:[ a_label_for id ]
            [ txt (field_to_string field |> CCString.capitalize_ascii) ]
        ]
    in
    let table =
      let link (id, field) =
        span
          ~a:[ a_id id; a_class [ "btn" ] ]
          [ txt
              Pool_common.(
                Utils.control_to_string
                  language
                  Pool_common.Message.(SelectAll (Some field)))
          ]
      in
      let thead =
        let open Pool_common.Message in
        txt ""
        :: ([ "all-showup", Field.ShowUp
            ; "all-participated", Field.Participated
            ]
           |> CCList.map link)
      in
      CCList.map
        (fun ({ Assignment.contact; _ } : Assignment.t) ->
          [ div [ strong [ txt (Contact.fullname contact) ] ]
          ; checkbox_element contact Message.Field.ShowUp
          ; checkbox_element ~disabled:true contact Message.Field.Participated
          ])
        assignments
      |> Table.horizontal_table
           ~classnames:[ "break-mobile"; "keep-head" ]
           ~thead
           `Striped
      |> fun table ->
      form
        ~a:
          [ a_method `Post
          ; a_class [ "stack" ]
          ; a_action
              (Format.asprintf
                 "/admin/experiments/%s/sessions/%s/close"
                 (Id.value experiment.Experiment.id)
                 (Id.value session.Session.id)
              |> Sihl.Web.externalize_path)
          ]
        [ Input.csrf_element csrf ()
        ; table
        ; div
            ~a:[ a_class [ "flexrow"; "justify-end" ] ]
            [ Input.submit_element language control ~submit_type:`Primary () ]
        ]
    in
    let scripts =
      {js|
        const showUp = document.querySelectorAll('[name="show_up[]"]');
        const participated = document.querySelectorAll('[name="participated[]"]');
        for(let i = 0; i < showUp.length; i++) {
          let elm = showUp[i];
          let target = document.querySelector(`[name="participated[]"][value="${elm.value}"]`)
          elm.addEventListener("change", () => {
            target.disabled = !elm.checked;
          })
        }
        const toggleShowUp = document.getElementById("all-showup");
        toggleShowUp.addEventListener("click", () => {
          showUp.forEach( (elm) => {
            var event = new Event('change');
            elm.checked = true;
            elm.dispatchEvent(event);
          })
        })

        const toggleParticipated = document.getElementById("all-participated");
        toggleParticipated.addEventListener("click", () => {
          participated.forEach( (elm) => {
            if(!elm.disabled) {
              elm.checked = true;
            }
          })
        })
      |js}
    in
    div
      [ h4
          ~a:[ a_class [ "heading-4" ] ]
          [ txt
              Pool_common.(
                Utils.field_to_string language Message.Field.Participants
                |> CCString.capitalize_ascii)
          ]
      ; table
      ; script (Unsafe.data scripts)
      ]
  in
  div
    [ p
        [ txt
            (session
            |> session_title
            |> Pool_common.Utils.text_to_string language)
        ]
    ; form
    ]
  |> Page_admin_experiments.experiment_layout
       language
       (Page_admin_experiments.Control control)
       experiment
;;
