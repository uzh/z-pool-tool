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
  ?(duplicate : Session.t option)
  locations
  sys_languages
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
          (Id.value session.Session.id)
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
         (Reminder.LeadTime.value %> Utils.Time.timespan_spanpicker)
  in
  let to_default_value html =
    div ~a:[ a_class [ "gap"; "inset-sm"; "border-left" ] ] [ html ]
  in
  let action, submit =
    let open Pool_common in
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
          (follow_up_to.Session.id |> Id.value)
      , Message.(Create (Some Field.FollowUpSession)) )
    | Some session, _ ->
      ( Format.asprintf "%s/%s" base (session.id |> Id.value)
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
        [ (let value =
             if CCOption.is_some duplicate || CCOption.is_some follow_up_to
             then None
             else
               Some
                 (value (fun s -> s.start |> Start.value |> Ptime.to_rfc3339))
           in
           flatpicker_element
             language
             `Datetime_local
             Message.Field.Start
             ~required:true
             ~flash_fetcher
             ?value
             ~warn_past:true
             ~additional_attributes:
               (if has_assignments then [ a_disabled () ] else []))
        ; flatpicker_element
            language
            ~required:true
            `Time
            Message.Field.Duration
            ~help:I18n.TimeSpanPickerHint
            ~value:
              (value (fun s ->
                 s.duration |> Duration.value |> Utils.Time.timespan_spanpicker))
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
        ; location_select locations None ()
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
                [ flatpicker_element
                    language
                    `Time
                    Message.Field.LeadTime
                    ~help:I18n.TimeSpanPickerHint
                    ~value:
                      (value (fun s -> s.reminder_lead_time |> lead_time_value))
                    ~flash_fetcher
                ; experiment.Experiment.session_reminder_lead_time
                  |> CCOption.map_or ~default:(txt "") (fun leadtime ->
                       Utils.text_to_string
                         language
                         (I18n.SessionReminderDefaultLeadTime
                            (leadtime |> Reminder.LeadTime.value))
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
                Message.Field.ReminderSubject
                ~value:
                  (value (fun s ->
                     s.reminder_subject
                     |> CCOption.map_or ~default:"" Reminder.Subject.value))
                ~flash_fetcher
            ; experiment.Experiment.session_reminder_subject
              |> CCOption.map_or ~default:(txt "") (fun text ->
                   Utils.text_to_string
                     language
                     (I18n.SessionReminderDefaultSubject
                        (text |> Reminder.Subject.value))
                   |> Http_utils.add_line_breaks
                   |> to_default_value)
            ; textarea_element
                language
                Message.Field.ReminderText
                ~value:
                  (value (fun s ->
                     s.reminder_text
                     |> CCOption.map_or ~default:"" Reminder.Text.value))
                ~flash_fetcher
            ; experiment.Experiment.session_reminder_text
              |> CCOption.map_or ~default:(txt "") (fun text ->
                   Utils.text_to_string
                     language
                     (I18n.SessionReminderDefaultText
                        (text |> Reminder.Text.value))
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
  let open Pool_common in
  let action =
    Format.asprintf
      "/admin/experiments/%s/sessions/%s/reschedule"
      (Experiment.Id.value experiment.Experiment.id)
      (Id.value session.Session.id)
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
        Message.Field.Start
        ~required:true
        ~flash_fetcher
        ~value:(session.start |> Start.value |> Ptime.to_rfc3339)
        ~disable_past:true
    ; flatpicker_element
        language
        ~required:true
        `Time
        Message.Field.Duration
        ~help:I18n.TimeSpanPickerHint
        ~value:
          (session.duration |> Duration.value |> Utils.Time.timespan_spanpicker)
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
  |> Page_admin_experiments.experiment_layout
       language
       (Page_admin_experiments.Control Message.(Reschedule (Some Field.Session)))
       experiment
;;

let index
  Pool_context.{ language; csrf; _ }
  experiment
  grouped_sessions
  chronological
  =
  let open Pool_common in
  let experiment_id = experiment.Experiment.id in
  let add_session_btn =
    link_as_button
      ~style:`Success
      ~icon:`Add
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
        let session_row session =
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
                 Utils.Time.formatted_date_time t)
            |> txt
          ; div
              ~a:[ a_class [ "flexrow"; "flex-gap"; "justify-end" ] ]
              [ Format.asprintf
                  "/admin/experiments/%s/sessions/%s"
                  (Experiment.Id.value experiment_id)
                  (Id.value session.id)
                |> edit_link
              ; delete_form
              ]
          ]
        in
        session_row parent :: CCList.map session_row follow_ups)
      grouped_sessions
  in
  let thead =
    Message.(
      [ Field.Date; Field.AssignmentCount; Field.CanceledAt ]
      |> Table.fields_to_txt language)
    @ [ add_session_btn ]
  in
  let html =
    div
      ~a:[ a_class [ "stack" ] ]
      [ p [ I18n.SessionIndent |> Utils.text_to_string language |> txt ]
      ; a
          ~a:
            [ a_href
                (Format.asprintf
                   "/admin/experiments/%s/sessions%s"
                   (Experiment.Id.value experiment_id)
                   (if chronological then "" else "?chronological=true")
                |> Sihl.Web.externalize_path)
            ]
          [ p
              [ (if chronological
                then I18n.SwitchGrouped
                else I18n.SwitchChronological)
                |> Utils.text_to_string language
                |> txt
              ]
          ]
        (* TODO [aerben] allow tables to be sorted generally? *)
      ; Table.horizontal_table `Striped ~align_last_end:true ~thead rows
      ]
  in
  Page_admin_experiments.experiment_layout
    ~hint:I18n.ExperimentSessions
    language
    (Page_admin_experiments.NavLink I18n.Sessions)
    experiment
    ~active:I18n.Sessions
    html
;;

let new_form
  Pool_context.{ language; csrf; _ }
  experiment
  duplicate_session
  locations
  sys_languages
  flash_fetcher
  =
  Page_admin_experiments.experiment_layout
    language
    (Page_admin_experiments.Control Message.(Create (Some Field.Session)))
    experiment
    (session_form
       csrf
       language
       experiment
       ?duplicate:duplicate_session
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
  let open Pool_common in
  let session_link (show, url, control) =
    match show with
    | false -> None
    | true ->
      link_as_button
        ~control:(language, control)
        ~classnames:[ "small" ]
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
      Message.
        [ ( CCOption.is_none session.follow_up_to
          , "follow-up"
          , Create (Some Field.FollowUpSession) )
        ; ( session.assignment_count |> AssignmentCount.value > 0
            && CCOption.is_none session.closed_at
          , "reschedule"
          , Reschedule (Some Field.Session) )
        ; ( session |> is_cancellable |> CCResult.is_ok
          , "cancel"
          , Cancel (Some Field.Session) )
        ; ( session |> is_closable |> CCResult.is_ok
          , "close"
          , Close (Some Field.Session) )
        ]
      |> CCList.filter_map session_link
    in
    let duplicate =
      let link =
        match session.follow_up_to with
        | Some parent_session ->
          Format.asprintf
            "/admin/experiments/%s/sessions/%s/follow-up?duplicate_id=%s"
            (Experiment.Id.value experiment.Experiment.id)
            (Pool_common.Id.value parent_session)
            (Pool_common.Id.value session.id)
        | None ->
          Format.asprintf
            "/admin/experiments/%s/sessions/create/?duplicate_id=%s"
            (Experiment.Id.value experiment.Experiment.id)
            (Pool_common.Id.value session.id)
      in
      link_as_button
        ~control:(language, Message.Duplicate (Some Field.Session))
        ~classnames:[ "small" ]
        link
    in
    div
      ~a:[ a_class [ "stack" ] ]
      [ table
      ; p ~a:[ a_class [ "flexrow"; "flex-gap" ] ] (links @ [ duplicate ])
      ]
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
          [ txt (Utils.nav_link_to_string language I18n.Assignments) ]
      ; assignment_list
      ]
  in
  let edit_button =
    link_as_button
      ~icon:`Create
      ~classnames:[ "small" ]
      ~control:(language, Message.(Edit (Some Field.Session)))
      (Format.asprintf
         "/admin/experiments/%s/sessions/%s/edit"
         (Experiment.Id.value experiment.Experiment.id)
         (Id.value session.id))
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
       (Page_admin_experiments.Control Message.(Edit (Some Field.Session)))
       experiment
;;

let follow_up
  Pool_context.{ language; csrf; _ }
  experiment
  duplicate_session
  (parent_session : Session.t)
  locations
  sys_languages
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
        ~follow_up_to:parent_session
        ?duplicate:duplicate_session
        locations
        sys_languages
        ~flash_fetcher
    ]
  |> Page_admin_experiments.experiment_layout
       language
       (Page_admin_experiments.Control
          Message.(Create (Some Field.FollowUpSession)))
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
  let form =
    let checkbox_element ?(disabled = false) contact field =
      let disabled = if disabled then [ a_disabled () ] else [] in
      div
        [ input
            ~a:
              ([ a_input_type `Checkbox
               ; a_name Message.Field.(array_key field)
               ; a_value (contact |> Contact.id |> Id.value)
               ]
              @ disabled)
            ()
        ]
    in
    let table =
      let link (id, label) = span ~a:[ a_id id ] [ txt label ] in
      let thead =
        txt ""
        :: ([ "all-showup", "S"; "all-participated", "P" ] |> CCList.map link)
      in
      CCList.map
        (fun ({ Assignment.contact; _ } : Assignment.t) ->
          [ div [ strong [ txt (Contact.fullname contact) ] ]
          ; checkbox_element contact Message.Field.ShowUp
          ; checkbox_element ~disabled:true contact Message.Field.Participated
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

        const isActive = (elm) => {
          return elm.dataset.active;
        }

        const toggleActive = (elm) => {
          if(isActive(elm)) {
            elm.removeAttribute("data-active");
          } else {
            elm.dataset.active = true;
          }
        }

        toggleShowUp.addEventListener("click", () => {
          showUp.forEach((elm) => {
            var event = new Event('change');
            elm.checked = !isActive(toggleShowUp);
            elm.dispatchEvent(event);
          });
          toggleActive(toggleShowUp);
        })

        const toggleParticipated = document.getElementById("all-participated");
        toggleParticipated.addEventListener("click", () => {
          participated.forEach((elm) => {
            if(!elm.disabled) {
              elm.checked = !isActive(toggleParticipated);
            }
          });
          toggleActive(toggleParticipated)
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
          [ Utils.hint_to_string language I18n.SessionClose
            |> HttpUtils.add_line_breaks
          ]
      ; table
      ; script (Unsafe.data scripts)
      ]
  in
  div
    [ p [ txt (session |> session_title |> Utils.text_to_string language) ]
    ; form
    ]
  |> Page_admin_experiments.experiment_layout
       language
       (Page_admin_experiments.Control control)
       experiment
;;

let cancel
  Pool_context.{ language; csrf; _ }
  experiment
  (session : Session.t)
  flash_fetcher
  =
  let open Pool_common in
  let action =
    Format.asprintf
      "/admin/experiments/%s/sessions/%s/cancel"
      (Experiment.Id.value experiment.Experiment.id)
      (Id.value session.Session.id)
  in
  (match Session.is_cancellable session with
   | Error reason -> p [ reason |> Utils.error_to_string language |> txt ]
   | Ok _ ->
     form
       ~a:
         [ a_class [ "stack" ]
         ; a_method `Post
         ; a_action (action |> Sihl.Web.externalize_path)
         ]
       [ csrf_element csrf ()
       ; textarea_element ~flash_fetcher language Message.Field.Reason
       ; span
           [ I18n.SessionCancelMessage |> Utils.hint_to_string language |> txt ]
       ; p [ I18n.NotifyVia |> Utils.text_to_string language |> txt ]
       ; checkbox_element ~flash_fetcher language Message.Field.Email
         (* TODO issue #149 re-add this *)
         (* ; checkbox_element ~flash_fetcher language Message.Field.SMS *)
       ; div
           ~a:[ a_class [ "flexrow" ] ]
           [ submit_element
               ~classnames:[ "push" ]
               language
               Message.(Cancel (Some Field.Session))
               ()
           ]
       ])
  |> Page_admin_experiments.experiment_layout
       language
       (Page_admin_experiments.Control Message.(Cancel (Some Field.Session)))
       experiment
;;
