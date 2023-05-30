open Entity

type event =
  | Sent of (t * Pool_tenant.t)
  | BulkSent of (t list * Pool_tenant.t)
[@@deriving eq, show]

let get_api_key tenant =
  let open Utils.Lwt_result.Infix in
  Pool_tenant.find_gtx_api_key tenant ||> Pool_common.Utils.get_or_failwith
;;

let handle_event pool : event -> unit Lwt.t = function
  | Sent (message, tenant) ->
    let%lwt api_key = get_api_key tenant in
    Text_message_service.send pool api_key message
  | BulkSent (messages, tenant) ->
    let%lwt api_key = get_api_key tenant in
    Lwt_list.iter_s (Text_message_service.send pool api_key) messages
;;
