open CCFun
open Tyxml.Html
module HttpUtils = Http_utils

let schedule_overview language schedules =
  let open Pool_common in
  let thead =
    Message.Field.[ Label; ScheduledTime; Status; LastRunAt ]
    |> Component.Table.fields_to_txt language
  in
  CCList.map
    (fun ({ Schedule.label; status; scheduled_time; last_run } :
           Schedule.public) ->
      let open Schedule in
      let scheduled =
        match scheduled_time with
        | Every span ->
          Utils.hint_to_string
            language
            (I18n.ScheduleEvery (ScheduledTimeSpan.value span))
        | At time ->
          Utils.hint_to_string
            language
            (I18n.ScheduleAt (ScheduledTime.value time))
      in
      [ txt (Label.value label)
      ; txt scheduled
      ; txt (Status.show status)
      ; txt
          (last_run
          |> CCOption.map_or
               ~default:"---"
               (LastRunAt.value %> Pool_common.Utils.Time.formatted_date_time))
      ])
    schedules
  |> Component.Table.horizontal_table `Striped ~align_last_end:true ~thead
;;

let index Pool_context.{ language; _ } schedules =
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt Pool_common.(Utils.nav_link_to_string language I18n.Schedules) ]
    ; p
        [ Pool_common.(Utils.hint_to_string language I18n.ScheduledIntro)
          |> HttpUtils.add_line_breaks
        ]
    ; schedule_overview language schedules
    ]
;;
