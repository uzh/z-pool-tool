open Entity

type event =
  | Sent of job
  | BulkSent of job list
  | ReportCreated of delivery_report
[@@deriving eq, show, variants]

let handle_event pool : event -> unit Lwt.t = function
  | Sent job -> Text_message_service.send pool job
  | BulkSent jobs -> Lwt_list.iter_s (Text_message_service.send pool) jobs
  | ReportCreated report -> Repo.insert_report pool report
;;
