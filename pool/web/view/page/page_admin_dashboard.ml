open Tyxml.Html

module Partials = struct
  let session_overview sessions =
    let open Session in
    let session_item session =
      div
        ~a:[ a_class [ "flexcolumn"; "gap-sm" ] ]
        [ div [ span [ txt (start_end_with_duration_human session) ] ]
        ; div [ txt Pool_location.(session.location.name |> Name.value) ]
        ]
    in
    sessions |> CCList.map session_item |> div ~a:[ a_class [ "stack" ] ]
  ;;
end

let index statistics incomplete_sessions Pool_context.{ language; _ } =
  let open Pool_common in
  let heading_2 title =
    h2
      ~a:[ a_class [ "heading-2" ] ]
      [ txt (Utils.text_to_string language title) ]
  in
  let calendar_html =
    div
      [ heading_2 I18n.UpcomingSessionsTitle; Component.Calendar.(create User) ]
  in
  let sessions_html =
    div
      [ heading_2 I18n.IncompleteSessions
      ; Partials.session_overview incomplete_sessions
      ]
  in
  let statistics_html =
    statistics
    |> CCOption.map_or ~default:(txt "") (fun statistics ->
      div
        [ heading_2 I18n.PoolStatistics
        ; Component.Statistics.create language statistics
        ])
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt (Utils.text_to_string language I18n.DashboardTitle) ]
    ; div
        ~a:[ a_class [ "stack-lg" ] ]
        [ calendar_html
        ; div ~a:[ a_class [ "grid-col-2" ] ] [ statistics_html; sessions_html ]
        ]
    ]
;;
