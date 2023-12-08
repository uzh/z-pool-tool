open Tyxml.Html
module HttpUtils = Http_utils

let formatted_date_time = Pool_common.Utils.Time.formatted_date_time

let queue_overview language queued_jobs =
  let open Pool_common in
  let thead =
    (Message.Field.[ Name; Status; Input; LastError; LastErrorAt; NextRunAt ]
     |> Component.Table.fields_to_txt language)
    @ [ txt "" ]
  in
  CCList.map
    (fun { Sihl_queue.id
         ; name
         ; status
         ; input
         ; last_error
         ; last_error_at
         ; next_run_at
         ; _
         } ->
      [ txt name
      ; txt (Sihl.Contract.Queue.show_instance_status status)
      ; txt (CCString.take 80 input)
      ; txt (CCOption.value ~default:"-" last_error)
      ; txt (last_error_at |> CCOption.map_or ~default:"-" formatted_date_time)
      ; txt (next_run_at |> formatted_date_time)
      ; Format.asprintf "/admin/settings/queue/%s" id
        |> Component.Input.link_as_button ~icon:Component.Icon.Eye
      ])
    queued_jobs
  |> Component.Table.horizontal_table `Striped ~align_last_end:true ~thead
;;

let job_detail
  language
  { Sihl_queue.name
  ; input
  ; tries
  ; next_run_at
  ; max_tries
  ; status
  ; last_error
  ; last_error_at
  ; tag
  ; _
  }
  =
  let default = "-" in
  let vertical_table =
    Component.Table.vertical_table
      ~classnames:[ "layout-fixed" ]
      ~align_top:true
      `Striped
      language
  in
  let job_detail =
    let open Pool_common.Message in
    [ Field.Name, name |> txt
    ; ( Field.Input
      , input
        |> Yojson.Safe.from_string
        |> Yojson.Safe.pretty_to_string
        |> HttpUtils.add_line_breaks )
    ; Field.Tag, tag |> CCOption.value ~default |> txt
    ; Field.Tries, tries |> CCInt.to_string |> txt
    ; Field.MaxTries, max_tries |> CCInt.to_string |> txt
    ; Field.Status, status |> Sihl.Contract.Queue.show_instance_status |> txt
    ; ( Field.LastErrorAt
      , last_error_at |> CCOption.map_or ~default formatted_date_time |> txt )
    ; Field.LastError, last_error |> CCOption.value ~default |> txt
    ; Field.NextRunAt, next_run_at |> formatted_date_time |> txt
    ]
    |> vertical_table
  in
  div [ job_detail ]
;;

let layout language children =
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt Pool_common.(Utils.nav_link_to_string language I18n.Queue) ]
    ; children
    ]
;;

let index Pool_context.{ language; _ } jobs =
  queue_overview language jobs |> layout language
;;

let detail Pool_context.{ language; _ } job =
  job_detail language job |> layout language
;;
