open Tyxml.Html
open Component
module Field = Pool_common.Message.Field
module Time = Pool_common.Utils.Time

let session_title language (s : Session.Public.t) =
  Pool_common.I18n.SessionDetailTitle
    (s.Session.Public.start |> Session.Start.value)
  |> Pool_common.Utils.text_to_string language
;;

let session_item layout language experiment session =
  let open Experiment.Public in
  let open Pool_common in
  let link =
    match
      ( Session.Public.is_fully_booked session
      , session.Session.Public.follow_up_to )
    with
    | false, None ->
      a
        ~a:
          [ a_href
              (Format.asprintf
                 "/experiments/%s/sessions/%s"
                 (experiment.id |> Experiment.Id.value)
                 (session.Session.Public.id |> Id.value)
              |> Sihl.Web.externalize_path)
          ]
        [ txt (Utils.control_to_string language Message.register) ]
    | false, Some _ ->
      span
        [ txt
            (Utils.error_to_string
               language
               Message.SessionRegistrationViaParent)
        ]
    | true, _ ->
      span [ txt (Utils.error_to_string language Message.SessionFullyBooked) ]
  in
  let attrs =
    if CCOption.is_some session.Session.Public.follow_up_to
    then [ a_class [ "inset"; "left" ] ]
    else []
  in
  [ div
      ~a:attrs
      [ txt
          Session.(
            session.Session.Public.start
            |> Start.value
            |> Time.formatted_date_time)
      ]
  ; txt
      Session.(
        session.Session.Public.duration
        |> Duration.value
        |> Time.formatted_timespan)
  ; session.Session.Public.location |> Component.Location.preview language
  ]
  |> fun cells ->
  match layout with
  | `Upcoming -> cells
  | `Register -> cells @ [ link ]
;;

let public_overview sessions experiment language =
  let thead = Field.[ Some Start; Some Duration; Some Location; None ] in
  let session_item = session_item `Register language experiment in
  CCList.flat_map
    (fun (session, follow_ups) ->
      session_item session :: CCList.map session_item follow_ups)
    sessions
  |> Component.Table.responsive_horizontal_table
       `Striped
       language
       ~align_last_end:true
       thead
;;

let public_detail language =
  let open Session in
  let rows session =
    [ ( Field.Start
      , session.Public.start |> Start.value |> Time.formatted_date_time |> txt )
    ; ( Field.Duration
      , session.Public.duration
        |> Duration.value
        |> Time.formatted_timespan
        |> txt )
    ; ( Field.Description
      , CCOption.map_or ~default:"" Description.value session.Public.description
        |> txt )
    ; ( Field.Location
      , session.Session.Public.location
        |> Partials.location_to_html ~public:true language )
    ]
  in
  CCList.flat_map (fun session ->
    [ h2
        ~a:[ a_class [ "heading-2" ] ]
        [ session |> session_title language |> txt ]
    ; Table.vertical_table `Striped language ~align_top:true (rows session)
    ])
;;
