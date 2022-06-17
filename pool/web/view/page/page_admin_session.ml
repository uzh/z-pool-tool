open Tyxml.Html
open Component
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

let create csrf language experiment_id ?session locations ~flash_fetcher =
  let action, submit =
    let open Pool_common in
    let base =
      Format.asprintf
        "/admin/experiments/%s/sessions"
        (Pool_common.Id.value experiment_id)
    in
    match session with
    | None -> base, Message.(Update (Some Field.Session))
    | Some session ->
      ( Format.asprintf "%s/%s" base (session.Session.id |> Pool_common.Id.value)
      , Message.(Create (Some Field.Session)) )
  in
  div
    [ h1
        ~a:[ a_class [ "heading-2" ] ]
        [ txt
            Pool_common.(
              Utils.control_to_string
                language
                Message.(Create (Some Field.Session)))
        ]
    ; form
        ~a:
          [ a_class [ "stack" ]
          ; a_method `Post
          ; a_action (action |> Sihl.Web.externalize_path)
          ]
        [ Component.csrf_element csrf ()
        ; flatpicker_element
            language
            `Datetime_local
            Pool_common.Message.Field.Start
            ~required:true
            ~flash_fetcher
            ~warn_past:true
        ; flatpicker_element
            language
            ~required:true
            `Time
            Pool_common.Message.Field.Duration
            ~help:Pool_common.I18n.TimeSpanPickerHint
            ~flash_fetcher
        ; textarea_element
            language
            Pool_common.Message.Field.Description
            ~flash_fetcher
        ; location_select locations None ()
        ; input_element
            language
            `Number
            Pool_common.Message.Field.MaxParticipants
            ~required:true
            ~flash_fetcher
        ; input_element
            language
            `Number
            Pool_common.Message.Field.MinParticipants
            ~required:true
            ~flash_fetcher
            ~value:"0"
        ; input_element
            language
            `Number
            Pool_common.Message.Field.Overbook
            ~required:true
            ~flash_fetcher
        ; div
            ~a:[ a_class [ "gap-lg" ] ]
            [ h3
                ~a:[ a_class [ "heading-3" ] ]
                [ txt Pool_common.(Utils.text_to_string language I18n.Reminder)
                ]
            ; div
                ~a:[ a_class [ "stack" ] ]
                [ input_element
                    language
                    `Number
                    Pool_common.Message.Field.LeadTime
                    ~help:Pool_common.I18n.NumberIsSecondsHint
                    ~required:true
                    ~flash_fetcher
                ; textarea_element
                    language
                    Pool_common.Message.Field.ReminderText
                    ~flash_fetcher
                ]
            ]
        ; submit_element language submit ()
        ]
    ]
;;

let index
    Pool_context.{ language; csrf; _ }
    experiment
    sessions
    locations
    flash_fetcher
  =
  let experiment_id = experiment.Experiment.id in
  let rows =
    CCList.map
      (fun (session : Session.t) ->
        let open Session in
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
              [ Component.csrf_element csrf ()
              ; submit_element language Message.(Cancel None) ()
              ]
        in
        [ txt (session |> Session.session_date_to_human)
        ; txt
            (CCInt.to_string
               (session.Session.assignment_count
               |> Session.AssignmentCount.value))
        ; session.Session.canceled_at
          |> CCOption.map_or ~default:"" (fun t ->
                 Pool_common.Utils.Time.formatted_date_time t)
          |> txt
        ; a
            ~a:
              [ a_href
                  (Format.asprintf
                     "/admin/experiments/%s/sessions/%s"
                     (Pool_common.Id.value experiment_id)
                     (Pool_common.Id.value session.id)
                  |> Sihl.Web.externalize_path)
              ]
            [ txt Pool_common.(Utils.control_to_string language Message.(More))
            ]
        ; cancel_form
        ; form
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
            [ Component.csrf_element csrf ()
            ; submit_element
                language
                Message.(Delete None)
                ~submit_type:`Error
                ()
            ]
        ])
      sessions
  in
  let thead =
    Pool_common.Message.Field.
      [ Some Date; Some AssignmentCount; Some CanceledAt; None; None; None ]
  in
  let html =
    div
      ~a:[ a_class [ "stack-lg" ] ]
      [ Table.horizontal_table `Striped language ~thead rows
      ; create csrf language experiment_id locations ~flash_fetcher
      ]
  in
  Page_admin_experiments.experiment_layout
    language
    (Page_admin_experiments.NavLink Pool_common.I18n.Sessions)
    experiment.Experiment.id
    ~active:Pool_common.I18n.Sessions
    html
;;

let detail
    (Pool_context.{ language; _ } as context)
    experiment_id
    (session : Session.t)
    assignments
  =
  let open Session in
  let session_overview =
    div
      ~a:[ a_class [ "stack" ] ]
      [ (let rows =
           let amount amt = amt |> ParticipantAmount.value |> string_of_int in
           let open Message in
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
             , Pool_location.to_string language session.Session.location |> txt
             )
           ; Field.MaxParticipants, amount session.max_participants |> txt
           ; Field.MinParticipants, amount session.min_participants |> txt
           ; Field.Overbook, amount session.overbook |> txt
           ]
           |> fun rows ->
           match session.canceled_at with
           | None -> rows
           | Some canceled ->
             rows
             @ [ ( Field.CanceledAt
                 , Pool_common.Utils.Time.formatted_date_time canceled |> txt )
               ]
         in
         Table.vertical_table `Striped language rows)
      ; p
          [ a
              ~a:
                [ a_href
                    (Format.asprintf
                       "/admin/experiments/%s/sessions/%s/edit"
                       (Pool_common.Id.value experiment_id)
                       (Pool_common.Id.value session.id)
                    |> Sihl.Web.externalize_path)
                ]
              [ Message.(Edit (Some Field.Session))
                |> Pool_common.Utils.control_to_string language
                |> txt
              ]
          ]
      ]
  in
  let assignments_html =
    let assignment_list =
      Page_admin_assignments.Partials.overview_list
        context
        experiment_id
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
  let html =
    div ~a:[ a_class [ "stack-lg" ] ] [ session_overview; assignments_html ]
  in
  Page_admin_experiments.experiment_layout
    language
    (Page_admin_experiments.I18n (session_title session))
    experiment_id
    html
;;

let edit
    Pool_context.{ language; csrf; _ }
    experiment_id
    (session : Session.t)
    locations
    flash_fetcher
  =
  let open Session in
  let html =
    div
      [ p
          [ session
            |> session_title
            |> Pool_common.Utils.text_to_string language
            |> txt
          ]
      ; (let amount amt = amt |> ParticipantAmount.value |> string_of_int in
         form
           ~a:
             [ a_class [ "stack" ]
             ; a_method `Post
             ; a_action
                 (Format.asprintf
                    "/admin/experiments/%s/sessions/%s"
                    (Pool_common.Id.value experiment_id)
                    (Pool_common.Id.value session.id)
                 |> Sihl.Web.externalize_path)
             ]
           [ Component.csrf_element csrf ()
             (* TODO [aerben] use better formatted date *)
           ; flatpicker_element
               language
               `Datetime_local
               Pool_common.Message.Field.Start
               ~required:true
               ~value:(session.start |> Start.value |> Ptime.to_rfc3339)
               ~flash_fetcher
               ~warn_past:true
           ; flatpicker_element
               language
               `Time
               Pool_common.Message.Field.Duration
               ~help:Pool_common.I18n.TimeSpanPickerHint
               ~value:
                 (session.duration
                 |> Duration.value
                 |> Pool_common.Utils.Time.timespan_spanpicker)
               ~required:true
               ~flash_fetcher
           ; textarea_element
               language
               Pool_common.Message.Field.Description
               ~value:
                 (CCOption.map_or
                    ~default:""
                    Description.value
                    session.description)
               ~flash_fetcher
           ; location_select locations (Some session.location) ()
           ; input_element
               language
               `Number
               Pool_common.Message.Field.MaxParticipants
               ~required:true
               ~value:(amount session.max_participants)
               ~flash_fetcher
           ; input_element
               language
               `Number
               Pool_common.Message.Field.MinParticipants
               ~required:true
               ~value:(amount session.min_participants)
               ~flash_fetcher
           ; input_element
               language
               `Number
               ~help:Pool_common.I18n.Overbook
               Pool_common.Message.Field.Overbook
               ~required:true
               ~value:(amount session.overbook)
               ~flash_fetcher
           ; submit_element
               language
               Message.(Update (Some Field.Session))
               ~submit_type:`Success
               ()
           ])
      ]
  in
  Page_admin_experiments.experiment_layout
    language
    (Page_admin_experiments.Control
       Pool_common.Message.(Edit (Some Field.Session)))
    experiment_id
    html
;;
