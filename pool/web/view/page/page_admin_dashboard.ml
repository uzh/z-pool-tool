open Tyxml.Html
open Pool_common
module Field = Message.Field

module Partials = struct
  let session_overview language (sessions, query) =
    let open Session in
    let field_to_string field =
      Utils.field_to_string language field |> CCString.capitalize_ascii |> txt
    in
    let url = Uri.of_string "/admin/dashboard/incomplete-sessions" in
    let session_path session =
      Page_admin_session.session_path
        session.experiment.Experiment.id
        session.id
    in
    let data_table = Component.DataTable.create_meta url query language in
    let th_class = [ "w-4"; "w-4"; "w-4" ] in
    let cols =
      [ `column column_date
      ; `custom (field_to_string Field.Experiment)
      ; `custom (field_to_string Field.Location)
      ; `empty
      ]
    in
    let row ({ experiment; _ } as session) =
      [ txt (start_end_with_duration_human session)
      ; span
          ~a:[ a_class [ "word-break-all" ] ]
          [ txt Experiment.(experiment.title |> Title.value) ]
      ; txt Pool_location.(session.location.name |> Name.value)
      ; Component.(
          Input.link_as_button ~icon:Icon.OpenOutline (session_path session))
      ]
      |> CCList.map CCFun.(CCList.return %> td)
      |> tr
    in
    Component.DataTable.make
      ~th_class
      ~target_id:"incomplete-sessions-list"
      ~cols
      ~row
      data_table
      sessions
  ;;
end

let index statistics incomplete_sessions Pool_context.{ language; _ } =
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
    div
      [ heading_2 I18n.IncompleteSessions
      ; Partials.session_overview language incomplete_sessions
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
        ; div
            ~a:[ a_class [ "grid-col-3" ] ]
            [ statistics_html
            ; div ~a:[ a_class [ "span-2" ] ] [ incomplete_sessions_html ]
            ]
        ]
    ]
;;
