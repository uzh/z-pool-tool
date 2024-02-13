module JobName = struct
  module Core = struct
    let field = Pool_common.Message.Field.Name

    type t =
      | SendEmail [@name "send_email"] [@printer Utils.ppx_printer "send_email"]
      | SendTextMessage [@name "send_text_message"]
      [@printer Utils.ppx_printer "send_text_message"]
    [@@deriving enum, eq, ord, sexp_of, show { with_path = false }, yojson]
  end

  include Pool_common.Model.SelectorType (Core)
  include Core
end

module Status = struct
  module Core = struct
    let field = Pool_common.Message.Field.Status

    type t =
      | Pending [@name "pending"]
      | Succeeded [@name "succeeded"]
      | Failed [@name "failed"]
      | Cancelled [@name "cancelled"]
    [@@deriving enum, eq, ord, sexp_of, show { with_path = false }, yojson]
  end

  include Pool_common.Model.SelectorType (Core)
  include Core

  let sihl_queue_to_human status =
    status
    |> Sihl.Contract.Queue.show_instance_status
    |> CCString.replace ~which:`All ~sub:"Contract_queue." ~by:""
  ;;
end

open Pool_common.Message

let column_job_name = (Field.Name, "queue_jobs.name") |> Query.Column.create

let column_job_status =
  (Field.Status, "queue_jobs.status") |> Query.Column.create
;;

let column_last_error =
  (Field.LastError, "queue_jobs.last_error") |> Query.Column.create
;;

let column_last_error_at =
  (Field.LastErrorAt, "queue_jobs.last_error_at") |> Query.Column.create
;;

let column_next_run =
  (Field.NextRunAt, "queue_jobs.next_run_at") |> Query.Column.create
;;

let column_input = (Field.Input, "queue_jobs.input") |> Query.Column.create

let build_options all show =
  let languages = Pool_common.Language.all in
  all
  |> CCList.map (fun item ->
    let label =
      languages
      |> CCList.map (fun lang -> lang, show item |> CCString.capitalize_ascii)
    in
    let value = show item in
    Query.Filter.SelectOption.create label value)
;;

let job_name_filter =
  let open Query.Filter in
  let open JobName in
  let options = build_options all show in
  Condition.Human.Select (column_job_name, options)
;;

let job_status_filter =
  let open Query.Filter in
  let open Status in
  let options = build_options all show in
  Condition.Human.Select (column_job_status, options)
;;

let searchable_by = [ column_input ]

let sortable_by =
  [ column_job_name
  ; column_job_status
  ; column_last_error
  ; column_last_error_at
  ; column_next_run
  ]
;;

let filterable_by = Some [ job_name_filter; job_status_filter ]

let default_sort =
  Query.Sort.{ column = column_next_run; order = SortOrder.Descending }
;;

let default_query = Query.create ~sort:default_sort ()
