open Tyxml.Html

module Partials = struct
  let session_overview language sessions =
    let open Session in
    let session_path session =
      Page_admin_session.session_path
        session.experiment.Experiment.id
        session.id
    in
    let session_item ({ experiment; _ } as session) =
      div
        ~a:[ a_class [ "flexcolumn"; "gap-sm" ] ]
        [ div [ strong [ txt Experiment.(experiment.title |> Title.value) ] ]
        ; div [ txt (start_end_with_duration_human session) ]
        ; div [ txt Pool_location.(session.location.name |> Name.value) ]
        ; div
            [ a
                ~a:
                  [ a_href (session_path session |> Sihl.Web.externalize_path)
                  ; a_class [ "has-icon" ]
                  ]
                [ txt
                    Pool_common.(
                      Utils.control_to_string language Message.SessionDetails)
                ; Component.Icon.(to_html OpenOutline)
                ]
            ]
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
  let incomplete_sessions_html =
    let html =
      match CCList.is_empty incomplete_sessions with
      | true ->
        p
          [ txt
              Pool_common.(Utils.text_to_string language I18n.EmptyListGeneric)
          ]
      | false -> Partials.session_overview language incomplete_sessions
    in
    div [ heading_2 I18n.IncompleteSessions; html ]
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
        ; div
            ~a:[ a_class [ "grid-col-2" ] ]
            [ statistics_html; incomplete_sessions_html ]
        ]
    ]
;;
