open Tyxml.Html
open Component
module Field = Pool_message.Field
module Time = Pool_model.Time

let session_title language (s : Session.Public.t) =
  Pool_common.I18n.SessionDetailTitle (s.Session.Public.start |> Session.Start.value)
  |> Pool_common.Utils.text_to_string language
;;

let session_item layout language (experiment : Experiment.Public.t) session =
  let open Pool_common in
  let open Session in
  let link =
    match Public.is_fully_booked session, session.Public.follow_up_to with
    | false, None ->
      a
        ~a:
          [ a_href
              (Format.asprintf
                 "/experiments/%s/sessions/%s"
                 Experiment.(experiment |> Public.id |> Id.value)
                 (session.Public.id |> Id.value)
               |> Sihl.Web.externalize_path)
          ]
        [ txt (Utils.control_to_string language Pool_message.Control.Register) ]
    | false, Some _ ->
      span
        [ txt
            (Utils.error_to_string
               language
               Pool_message.Error.SessionRegistrationViaParent)
        ]
    | true, _ ->
      span [ txt (Utils.error_to_string language Pool_message.Error.SessionFullyBooked) ]
  in
  let attrs =
    if CCOption.is_some session.Public.follow_up_to && not (layout == `Upcoming)
    then [ a_class [ "inset"; "left" ] ]
    else []
  in
  [ div
      ~a:attrs
      ((if CCOption.is_some session.Public.canceled_at
        then
          [ strong [ txt Pool_common.(Utils.text_to_string language I18n.Canceled) ]
          ; br ()
          ]
        else [])
       @ [ txt (Session.Public.start_end_with_duration_human session) ])
  ; session.Public.location |> Component.Location.preview
  ]
  |> fun cells ->
  match layout with
  | `Upcoming -> cells
  | `Register -> cells @ [ link ]
;;

let public_overview sessions experiment language =
  let thead = Field.[ Some DateTime; Some Location; None ] in
  let session_item = session_item `Register language experiment in
  CCList.flat_map
    (fun (session, follow_ups) ->
       session_item session :: CCList.map session_item follow_ups)
    sessions
  |> Component.Table.responsive_horizontal_table
       `Striped
       language
       ~align_top:true
       ~align_last_end:true
       thead
;;

let public_detail language =
  let open Session in
  let rows session =
    [ Field.Start, session.Public.start |> Start.value |> Time.formatted_date_time |> txt
    ; ( Field.Duration
      , session.Public.duration |> Duration.value |> Time.formatted_timespan |> txt )
    ; ( Field.Description
      , CCOption.map_or ~default:"" PublicDescription.value session.Public.description
        |> txt )
    ; ( Field.Location
      , session.Session.Public.location |> Partials.location_to_html ~public:true language
      )
    ]
  in
  CCList.flat_map (fun session ->
    [ h3
        ~a:[ a_class [ "heading-3"; "has-gap" ] ]
        [ session |> session_title language |> txt ]
    ; Table.vertical_table `Striped language ~align_top:true (rows session)
    ])
;;

let list Pool_context.{ language; _ } (sessions, query) =
  let open Session in
  let url = Http_utils.Url.Contact.session_path () |> Uri.of_string in
  let data_table =
    Component.DataTable.create_meta
      ~search:Public.searchable_by
      ?filter:Public.filterable_by
      url
      query
      language
  in
  let th_class = [ "w-4"; "w-4"; "w-4" ] in
  let cols =
    let field = Pool_common.Utils.field_to_string language in
    Session.
      [ `column column_date
      ; `custom (txt (field Pool_message.Field.Location))
      ; `custom (txt (field Pool_message.Field.Status))
      ; `empty
      ]
  in
  let status { Public.canceled_at; closed_at; _ } =
    let open Component.Tag in
    let open Pool_common in
    let open CCOption in
    let create_chip style text = create_chip ~inline:true style text |> return in
    [ is_some canceled_at, I18n.Canceled, `Error
    ; is_some closed_at, I18n.Closed, `Success
    ]
    |> CCList.find_map (fun (check, text, style) ->
      match check with
      | true -> create_chip style (Utils.text_to_string language text)
      | false -> None)
  in
  let row session =
    [ txt (Public.start_end_with_duration_human session), Some Field.Start
    ; ( txt (Pool_location.Name.value session.Public.location.Pool_location.name)
      , Some Field.Location )
    ; status session |> CCOption.value ~default:(txt ""), None
    ; txt "btn", None (* TODO: Button *)
    ]
    |> CCList.map (fun (html, field) ->
      let label = Component.Table.data_label_opt language field in
      td ~a:label [ html ])
    |> tr
  in
  Component.DataTable.make
    ~break_mobile:true
    ~th_class
    ~target_id:"session-table"
    ~cols
    ~row
    data_table
    sessions
;;

let index context title sessions =
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1 ~a:[ a_class [ "heading-1"; "has-gap" ] ] [ txt (I18n.content_to_string title) ]
    ; list context sessions
    ]
;;
