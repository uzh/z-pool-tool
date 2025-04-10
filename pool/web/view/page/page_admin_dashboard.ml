open Tyxml.Html
open Pool_common
module Field = Pool_message.Field

type incomplete_sessions = Session.t list * Query.t
type upcoming_sessions = Session.t list * Query.t

type layout =
  | Clean of incomplete_sessions
  | Admin of incomplete_sessions * upcoming_sessions

module Partials = struct
  let session_overview (target_id, url) language (sessions, query) =
    let open Session in
    let field_to_string field =
      Utils.field_to_string language field |> CCString.capitalize_ascii |> txt
    in
    let url = url |> Format.asprintf "/admin/dashboard/%s" |> Uri.of_string in
    let session_path session =
      Http_utils.Url.Admin.session_path ~id:session.id session.experiment.Experiment.id
    in
    let data_table = Component.DataTable.create_meta url query language ~push_url:false in
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
      [ txt (start_end_with_duration_human session), Some Field.Start
      ; ( span
            ~a:[ a_class [ "word-wrap-break-all" ] ]
            [ txt Experiment.(experiment.title |> Title.value) ]
        , Some Field.Experiment )
      ; txt Pool_location.(session.location.name |> Name.value), Some Field.Location
      ; Component.(Input.link_as_button ~icon:Icon.Eye (session_path session)), None
      ]
      |> CCList.map (fun (html, label) ->
        let attrs = Component.Table.data_label_opt language label in
        td ~a:attrs [ html ])
      |> tr ~a:row_attribs
    in
    Component.DataTable.make
      ~break_mobile:true
      ~th_class
      ~target_id
      ~cols
      ~row
      data_table
      sessions
  ;;

  let incomplete_sessions_list =
    session_overview ("incomplete-sessions-list", "incomplete-sessions")
  ;;

  let upcoming_sessions_list =
    session_overview ("upcoming-sessions-list", "upcoming-sessions")
  ;;
end

let index statistics duplicate_contacts_count layout Pool_context.{ language; _ } =
  let heading_2 title =
    h2
      ~a:[ a_class [ "heading-2"; "has-gap" ] ]
      [ txt (Utils.text_to_string language title) ]
  in
  let information_section incomplete_sessions =
    let statistics_html =
      statistics
      |> CCOption.map_or ~default:(txt "") (fun statistics ->
        div
          [ heading_2 I18n.PoolStatistics
          ; Component.Statistics.Pool.create language statistics
          ])
    in
    let incomplete_sessions_html =
      div
        [ heading_2 I18n.IncompleteSessions
        ; Partials.incomplete_sessions_list language incomplete_sessions
        ]
    in
    div
      ~a:[ a_class [ "grid-col-3"; "stretch-only-child"; "grid-gap-lg" ] ]
      [ statistics_html; div ~a:[ a_class [ "span-2" ] ] [ incomplete_sessions_html ] ]
  in
  let upcoming_section children =
    div
      [ heading_2 I18n.UpcomingSessionsTitle; div ~a:[ a_class [ "stack-lg" ] ] children ]
  in
  let duplicate_contacts =
    match duplicate_contacts_count with
    | None | Some 0 -> txt ""
    | Some duplicates ->
      let open Pool_common in
      [ p
          [ Utils.hint_to_string
              language
              (I18n.DashboardDuplicateContactsNotification duplicates)
            |> txt
          ]
      ]
      |> Component.Notification.create
           ~link:(Http_utils.Url.Admin.duplicate_path (), I18n.ManageDuplicates)
           language
           `Warning
  in
  let calendar_html = Component.Calendar.(create User) in
  let html =
    match layout with
    | Clean incomplete_sessions ->
      [ duplicate_contacts
      ; information_section incomplete_sessions
      ; upcoming_section [ calendar_html ]
      ]
    | Admin (incomplete_sessions, upcoming_sessions) ->
      let upcoming_sessions =
        let sessions =
          div
            ~a:[ a_class [ "stack" ] ]
            [ Page_admin_session.Partials.table_legend ~hide_closed:true language
            ; Partials.upcoming_sessions_list language upcoming_sessions
            ]
        in
        upcoming_section [ sessions; calendar_html ]
      in
      [ duplicate_contacts; upcoming_sessions; information_section incomplete_sessions ]
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1"; "has-gap" ] ]
        [ txt (Utils.text_to_string language I18n.DashboardTitle) ]
    ; div ~a:[ a_class [ "stack-xl" ] ] html
    ]
;;
