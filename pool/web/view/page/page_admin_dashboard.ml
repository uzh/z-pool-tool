open Tyxml.Html
open Pool_common
module Field = Message.Field

module Partials = struct
  let session_overview (target_id, url) language (sessions, query) =
    let open Session in
    let field_to_string field =
      Utils.field_to_string language field |> CCString.capitalize_ascii |> txt
    in
    let url = url |> Format.asprintf "/admin/dashboard/%s" |> Uri.of_string in
    let session_path session =
      Page_admin_session.session_path
        session.experiment.Experiment.id
        session.id
    in
    let data_table =
      Component.DataTable.create_meta url query language ~push_url:false
    in
    let th_class = [ "w-4"; "w-4"; "w-4" ] in
    let cols =
      [ `column column_date
      ; `custom (field_to_string Field.Experiment)
      ; `custom (field_to_string Field.Location)
      ; `empty
      ]
    in
    let row ({ experiment; _ } as session) =
      let row_attribs =
        if CCOption.is_some session.canceled_at
        then [ a_class [ "bg-red-lighter" ] ]
        else []
      in
      [ txt (start_end_with_duration_human session)
      ; span
          ~a:[ a_class [ "word-break-all" ] ]
          [ txt Experiment.(experiment.title |> Title.value) ]
      ; txt Pool_location.(session.location.name |> Name.value)
      ; Component.(
          Input.link_as_button ~icon:Icon.OpenOutline (session_path session))
      ]
      |> CCList.map CCFun.(CCList.return %> td)
      |> tr ~a:row_attribs
    in
    Component.DataTable.make ~th_class ~target_id ~cols ~row data_table sessions
  ;;

  let incomplete_sessions_list =
    session_overview ("incomplete-sessions-list", "incomplete-sessions")
  ;;

  let upcoming_sessions_list =
    session_overview ("upcoming-sessions-list", "upcoming-sessions")
  ;;
end

let index
  statistics
  upcoming_sessions
  incomplete_sessions
  recruiter_layout
  Pool_context.{ language; _ }
  =
  let heading_2 title =
    h2
      ~a:[ a_class [ "heading-2" ] ]
      [ txt (Utils.text_to_string language title) ]
  in
  let upcoming_sessions_html =
    let calendar_html = Component.Calendar.(create User) in
    let session_list =
      Partials.upcoming_sessions_list language upcoming_sessions
    in
    let elements = [ session_list; calendar_html ] in
    let html = if recruiter_layout then CCList.rev elements else elements in
    div
      [ heading_2 I18n.UpcomingSessionsTitle
      ; div ~a:[ a_class [ "stack-lg" ] ] html
      ]
  in
  let recruiter_information =
    let statistics_html =
      statistics
      |> CCOption.map_or ~default:(txt "") (fun statistics ->
        div
          [ heading_2 I18n.PoolStatistics
          ; Component.Statistics.create language statistics
          ])
    in
    let incomplete_sessions_html =
      div
        [ heading_2 I18n.IncompleteSessions
        ; Partials.incomplete_sessions_list language incomplete_sessions
        ]
    in
    div
      ~a:[ a_class [ "grid-col-3"; "stretch-only-child" ] ]
      [ statistics_html
      ; div ~a:[ a_class [ "span-2" ] ] [ incomplete_sessions_html ]
      ]
  in
  let elements = [ upcoming_sessions_html; recruiter_information ] in
  let html = if recruiter_layout then CCList.rev elements else elements in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt (Utils.text_to_string language I18n.DashboardTitle) ]
    ; div ~a:[ a_class [ "stack-xl" ] ] html
    ]
;;
