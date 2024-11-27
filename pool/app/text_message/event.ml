open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Entity

type job =
  { job : t
  ; id : Pool_queue.Id.t option [@yojson.option]
  ; message_template : string option [@yojson.option]
  ; job_ctx : Pool_queue.job_ctx option [@yojson.option]
  }
[@@deriving eq, fields, show, yojson]

let create_job ?id ?message_template ?job_ctx job = { job; id; message_template; job_ctx }

type event =
  | Sent of (job * Pool_user.CellPhone.t option)
  | BulkSent of job list
  | ReportCreated of delivery_report
[@@deriving eq, show, variants]

let bulksent_opt jobs = if CCList.is_empty jobs then [] else [ BulkSent jobs ]
let sent ?new_recipient job = Sent (job, new_recipient)

let create_sent ?id ?message_template ?job_ctx ?new_recipient job =
  create_job ?id ?message_template ?job_ctx job |> sent ?new_recipient
;;

let handle_event pool : event -> unit Lwt.t = function
  | Sent ({ job; id; message_template; job_ctx }, new_recipient) ->
    Text_message_service.dispatch ?id ?new_recipient ?message_template ?job_ctx pool job
  | BulkSent [] -> Lwt.return_unit
  | BulkSent jobs ->
    Lwt_list.iter_s
      (fun { job; id; message_template; job_ctx } ->
        Text_message_service.dispatch ?id ?message_template ?job_ctx pool job)
      jobs
  | ReportCreated report -> Repo.insert_report pool report
;;
