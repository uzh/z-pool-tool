open Tyxml.Html

let public_overview sessions experiment language =
  let open Experiment.Public in
  let thead =
    Pool_common.Message.Field.[ Some Start; Some Duration; Some Location; None ]
  in
  CCList.map
    (fun (session : Session.Public.t) ->
      [ txt
          Session.(
            session.Session.Public.start
            |> Start.value
            |> Pool_common.Utils.Time.formatted_date_time)
      ; txt
          Session.(
            session.Session.Public.duration
            |> Duration.value
            |> Pool_common.Utils.print_time_span)
      ; txt (session.Session.Public.location |> Pool_location.to_string language)
      ; (match Session.Public.is_fully_booked session with
        | false ->
          a
            ~a:
              [ a_href
                  (Format.asprintf
                     "/experiments/%s/sessions/%s"
                     (experiment.id |> Pool_common.Id.value)
                     (session.Session.Public.id |> Pool_common.Id.value))
              ]
            [ txt Pool_common.(Utils.control_to_string language Message.signup)
            ]
        | true ->
          span
            [ txt
                Pool_common.(
                  Utils.error_to_string language Message.SessionFullyBooked)
            ])
      ])
    sessions
  |> Component.Table.horizontal_table `Striped language ~thead
;;

let public_detail (session : Session.Public.t) language =
  let open Session in
  let open Pool_common.Message in
  let rows =
    [ ( Field.Start
      , session.Public.start
        |> Start.value
        |> Pool_common.Utils.Time.formatted_date_time )
    ; ( Field.Duration
      , session.Public.duration
        |> Duration.value
        |> Pool_common.Utils.print_time_span )
    ; ( Field.Description
      , CCOption.map_or ~default:"" Description.value session.Public.description
      )
    ; ( Field.Location
      , session.Session.Public.location |> Pool_location.to_string language )
    ; ( Field.CanceledAt
      , CCOption.map_or
          ~default:"Not canceled"
          (Ptime.to_rfc3339 ~space:true)
          session.Public.canceled_at )
    ]
    |> CCList.map (fun (field, value) ->
           tr
             [ th
                 [ txt
                     (field
                     |> Pool_common.Utils.field_to_string language
                     |> CCString.capitalize_ascii)
                 ]
             ; td [ txt value ]
             ])
  in
  table ~a:[ a_class [ "table"; "striped" ] ] rows
;;
