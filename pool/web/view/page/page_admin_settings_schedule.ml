open CCFun
open Tyxml.Html
module HttpUtils = Http_utils

let list Pool_context.{ language; _ } schedules query =
  let open Pool_common in
  let url = Uri.of_string "/admin/settings/schedules" in
  let data_table = Component.DataTable.create_meta url query language in
  let cols =
    [ `column Schedule.column_label
    ; `column Schedule.column_scheduled_time
    ; `column Schedule.column_status
    ; `column Schedule.column_last_run_at
    ]
  in
  let th_class = [ "w-5"; "w-2"; "w-2"; "w-3" ] in
  let row (schedule : Schedule.public) =
    let open Schedule in
    let scheduled =
      match schedule.scheduled_time with
      | Every span ->
        Utils.hint_to_string language (I18n.ScheduleEvery (ScheduledTimeSpan.value span))
      | At time ->
        Utils.hint_to_string language (I18n.ScheduleAt (ScheduledTime.value time))
    in
    [ txt (Label.value schedule.label)
    ; txt scheduled
    ; txt (Status.show schedule.status)
    ; txt
        (schedule.last_run
         |> CCOption.map_or
              ~default:"---"
              (LastRunAt.value %> Pool_model.Time.formatted_date_time))
    ]
    |> CCList.map (CCList.return %> td)
    |> tr
  in
  Component.DataTable.make
    ~th_class
    ~target_id:"schedule-table"
    ~cols
    ~row
    data_table
    schedules
;;

let index (Pool_context.{ language; _ } as context) schedules query =
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt Pool_common.(Utils.nav_link_to_string language I18n.Schedules) ]
    ; p
        [ Pool_common.(Utils.hint_to_string language I18n.ScheduledIntro)
          |> HttpUtils.add_line_breaks
        ]
    ; list context schedules query
    ]
;;
