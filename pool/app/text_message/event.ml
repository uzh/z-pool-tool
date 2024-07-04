open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Entity

type job =
  { job : t
  ; id : Pool_queue.Id.t option [@yojson.option]
  ; message_template : string option [@yojson.option]
  ; mappings : Pool_queue.mappings option [@yojson.option]
  }
[@@deriving eq, fields, show, yojson]

let create_job ?id ?message_template ?mappings job =
  { job; id; message_template; mappings }
;;

type event =
  | Sent of (job * Pool_user.CellPhone.t option)
  | BulkSent of job list
[@@deriving eq, show, variants]

let sent ?new_recipient job = Sent (job, new_recipient)

let create_sent ?id ?message_template ?mappings ?new_recipient job =
  create_job ?id ?message_template ?mappings job |> sent ?new_recipient
;;

let handle_event pool : event -> unit Lwt.t = function
  | Sent ({ job; id; message_template; mappings }, new_recipient) ->
    Text_message_service.dispatch
      ?id
      ?new_recipient
      ?message_template
      ?mappings
      pool
      job
  | BulkSent jobs ->
    Lwt_list.iter_s
      (fun { job; id; message_template; mappings } ->
        Text_message_service.dispatch ?id ?message_template ?mappings pool job)
      jobs
;;
