open Entity

type event =
  | Sent of t
  | BulkSent of t list
[@@deriving eq, show, variants]

let handle_event pool : event -> unit Lwt.t = function
  | Sent message -> Text_message_service.send pool message
  | BulkSent messages ->
    Lwt_list.iter_s (Text_message_service.send pool) messages
;;
