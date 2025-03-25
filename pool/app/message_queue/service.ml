open Utils.Lwt_result.Infix
open Amqp_client_lwt

module type Config = sig
  val host : string
  val credentials : string * string
end

module type Sig = sig
  module Queue : sig
    val create : Database.Label.t -> Amqp.Queue.t option Lwt.t
    val update : Database.Label.t -> unit Lwt.t
  end

  val init : Database.Label.t list -> unit Lwt.t
  val close : unit -> unit Lwt.t
  val dispatch : Database.Label.t -> message_id:string -> payload:string -> unit Lwt.t
end

module Make (C : Config) = struct
  let channel = ref None
  let connection = ref None

  module QueueCache = struct
    open CCFun.Infix
    open Hashtbl

    let tbl : (Database.Label.t, Queue.t) t = create 5
    let add = add tbl
    let find_opt = find_opt tbl

    let find_exn =
      find_opt
      %> function
      | Some instance -> instance
      | None -> failwith "RabbitMQ instance is not initialized"
    ;;

    let clear () = reset tbl
  end

  let with_connection (f : 'a Channel.t -> 'b Lwt.t) =
    let connect () =
      let%lwt new_connection =
        Connection.connect ~id:"email_service" ~credentials:C.credentials C.host
      in
      connection := Some new_connection;
      Lwt.return new_connection
    in
    let open_channel connection =
      let%lwt new_channel =
        Connection.open_channel ~id:"email_channel" Channel.with_confirm connection
      in
      channel := Some new_channel;
      Lwt.return new_channel
    in
    match !connection, !channel with
    | Some _, Some channel -> f channel
    | Some connection, None -> open_channel connection >|> f
    | None, Some channel ->
      let%lwt () = Channel.close channel in
      let%lwt connection = connect () in
      open_channel connection >|> f
    | None, _ ->
      let%lwt connection = connect () in
      open_channel connection >|> f
  ;;

  let close () =
    let () = QueueCache.clear () in
    match !connection, !channel with
    | Some connection, Some channel ->
      let%lwt () = Channel.close channel in
      Connection.close connection
    | Some connection, None -> Connection.close connection
    | None, Some channel -> Channel.close channel
    | None, None -> Lwt.return_unit
  ;;

  module QueueName = struct
    type t =
      { queue_name : string
      ; dlx_name : string
      ; dlq_name : string
      ; temp_queue_name : string
      ; shovel_to_temp_name : string
      ; shovel_to_original_name : string
      }

    let create pool =
      let environment =
        Sihl.Configuration.read_string "SIHL_ENV" |> CCOption.value ~default:"development"
      in
      let create = Printf.sprintf "%s_%s_%s" (Database.Label.value pool) environment in
      { queue_name = create "queue"
      ; dlx_name = create "dlx"
      ; dlq_name = create "dlq"
      ; temp_queue_name = create "temp_queue"
      ; shovel_to_temp_name = create "shovel_to_temp"
      ; shovel_to_original_name = create "shovel_to_original"
      }
    ;;

    let to_remove_after_migration pool =
      let { temp_queue_name; shovel_to_temp_name; shovel_to_original_name; _ } =
        create pool
      in
      [ temp_queue_name; shovel_to_temp_name; shovel_to_original_name ]
    ;;

    let originals pool =
      let { queue_name; dlq_name; _ } = create pool in
      [ queue_name; dlq_name ]
    ;;
  end

  module Queue = struct
    let declare pool : Amqp.Queue.t Lwt.t =
      let { QueueName.queue_name; dlx_name; dlq_name; _ } = QueueName.create pool in
      with_connection (fun channel ->
        Queue.declare
          ~durable:true
          ~arguments:
            [ "x-queue-type", Amqp.Types.VLongstr "quorum"
            ; Queue.dead_letter_exchange dlx_name
            ; Queue.dead_letter_routing_key dlq_name
            ; Queue.message_ttl (1000 * 60 * 60 * 24) (* 24 hours *)
            ; Queue.max_length 3
            ]
          channel
          queue_name)
    ;;

    let create_dead_letter pool =
      let { QueueName.dlx_name; dlq_name; _ } = QueueName.create pool in
      with_connection (fun channel ->
        let%lwt dlx = Exchange.declare channel Exchange.direct_t ~durable:true dlx_name in
        let%lwt dlq =
          Queue.declare channel ~durable:true ~exclusive:false ~auto_delete:false dlq_name
        in
        let%lwt () = Queue.bind channel dlq dlx (`Queue dlq_name) in
        Lwt.return_unit)
    ;;

    let create pool =
      Lwt.catch
        (fun () ->
           let%lwt queue = declare pool in
           let%lwt () = create_dead_letter pool in
           Lwt.return_some queue)
        (fun _ -> Lwt.return_none)
    ;;

    let shovel_payload src dest =
      `Assoc
        [ "src-uri", `String "amqp://"
        ; "src-queue", `String src
        ; "dest-uri", `String "amqp://"
        ; "dest-queue", `String dest
        ]
      |> Yojson.Safe.to_string
      |> fun str -> Amqp.Types.VLongstr str
    ;;

    let to_temp channel pool =
      let { QueueName.queue_name; temp_queue_name; shovel_to_temp_name; _ } =
        QueueName.create pool
      in
      let%lwt temp_queue =
        Queue.declare
          ~durable:true
          ~exclusive:false
          ~auto_delete:false
          channel
          temp_queue_name
      in
      (* Move messages from the original queue to the temporary queue using a Shovel *)
      let shovel_to_temp_payload = shovel_payload queue_name temp_queue_name in
      let%lwt (_ : Amqp.Queue.t) =
        Queue.declare
          ~durable:true
          ~arguments:[ "x-shovel", shovel_to_temp_payload ]
          channel
          shovel_to_temp_name
      in
      (* Wait for the Shovel to move all messages *)
      let%lwt () = Lwt_unix.sleep 5.0 in
      Lwt.return [ temp_queue ]
    ;;

    let to_original channel pool =
      let { QueueName.queue_name; temp_queue_name; shovel_to_original_name; _ } =
        QueueName.create pool
      in
      (* Move messages back from the temporary queue to the original queue using a Shovel *)
      let shovel_to_original_config =
        `Assoc
          [ "src-uri", `String "amqp://"
          ; "src-queue", `String temp_queue_name
          ; "dest-uri", `String "amqp://"
          ; "dest-queue", `String queue_name
          ]
      in
      let shovel_to_original_payload =
        Amqp.Types.VLongstr (Yojson.Safe.to_string shovel_to_original_config)
      in
      let%lwt shovel_to_original =
        Queue.declare
          ~durable:true
          ~arguments:[ "x-shovel", shovel_to_original_payload ]
          channel
          shovel_to_original_name
      in
      (* Wait for the Shovel to move all messages back *)
      let%lwt () = Lwt_unix.sleep 5.0 in
      Lwt.return [ shovel_to_original ]
    ;;

    let cleanup_after_migration channel pool =
      Lwt_list.iter_s
        (fun queue -> Queue.declare ~passive:true channel queue >|> Queue.delete channel)
        (QueueName.to_remove_after_migration pool)
    ;;

    let cleanup_original channel pool =
      Lwt_list.iter_s
        (fun queue -> Queue.declare ~passive:true channel queue >|> Queue.delete channel)
        (QueueName.originals pool)
    ;;

    let delete channel = Lwt_list.iter_s (Queue.delete channel)

    let find_exn pool =
      QueueCache.find_opt pool
      |> function
      | Some queue -> queue
      | None -> failwith "RabbitMQ queue is not found"
    ;;

    let migrate pool =
      with_connection (fun channel ->
        let%lwt _temp_queue = to_temp channel pool in
        let%lwt () = cleanup_original channel pool in
        let%lwt queue = declare pool in
        let%lwt () = create_dead_letter pool in
        let%lwt _shovel = to_original channel pool in
        let%lwt () = cleanup_after_migration channel pool in
        Lwt.return queue)
    ;;

    let update pool =
      let%lwt queue = migrate pool in
      let () = QueueCache.add pool queue in
      Lwt.return_unit
    ;;
  end

  let init = Lwt_list.iter_s Queue.update

  let dispatch pool ~message_id ~payload =
    let open Utils.Lwt_result.Infix in
    Logs.warn (fun m ->
      m "Dispatching email to RabbitMQ [%s]: %s" (Database.Label.value pool) message_id);
    Logs.warn (fun m -> m "Payload: %s" payload);
    Logs.warn (fun m ->
      m
        "Instance [%s]: %s"
        (Database.Label.value pool)
        (QueueCache.find_opt pool
         |> CCOption.map (fun _ -> "found")
         |> CCOption.value ~default:"not found"));
    let queue = Queue.find_exn pool in
    let message = Message.make ~message_id payload in
    with_connection (fun channel ->
      Amqp_client_lwt.Queue.publish channel queue message
      >|> function
      | `Ok -> Lwt.return_unit
      | `Failed -> Lwt.fail_with "Failed to publish message to RabbitMQ queue")
  ;;
end
