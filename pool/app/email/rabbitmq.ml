open Utils.Lwt_result.Infix
open Amqp_client_lwt

module type Config = sig
  val host : string
  val credentials : string * string
end

module Make (C : Config) = struct
  module Instance = struct
    type t =
      { connection : Connection.t
      ; channel : Channel.with_confirm Channel.t
      ; queue : Queue.t
      }

    let create connection channel queue = { connection; channel; queue }
  end

  module InstanceCache = struct
    open CCFun.Infix
    open Hashtbl

    let tbl : (Database.Label.t, Instance.t) t = create 5
    let add = add tbl
    let find_opt = find_opt tbl

    let find_exn =
      find_opt
      %> function
      | Some instance -> instance
      | None -> failwith "RabbitMQ instance is not initialized"
    ;;
  end

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
      let { queue_name; dlx_name; dlq_name; _ } = create pool in
      [ queue_name; dlx_name; dlq_name ]
    ;;
  end

  let create_queue channel pool =
    let { QueueName.queue_name; dlx_name; dlq_name; _ } = QueueName.create pool in
    Queue.declare
      ~durable:true
      ~arguments:
        [ "x-queue-type", Amqp.Types.VLongstr "quorum"
        ; Queue.dead_letter_exchange dlx_name
        ; Queue.dead_letter_routing_key dlq_name
        ; Queue.message_ttl (1000 * 60 * 60 * 12) (* 24 hours *)
        ; Queue.max_length 3
        ]
      channel
      queue_name
  ;;

  let create_dead_letter channel pool =
    let { QueueName.dlx_name; dlq_name; _ } = QueueName.create pool in
    let%lwt dlx = Exchange.declare channel Exchange.direct_t ~durable:true dlx_name in
    let%lwt dlq =
      Queue.declare channel ~durable:true ~exclusive:false ~auto_delete:false dlq_name
    in
    let%lwt () = Queue.bind channel dlq dlx (`Queue dlq_name) in
    Lwt.return_unit
  ;;

  let catch_create channel pool =
    Lwt.catch
      (fun () ->
         let%lwt queue = create_queue channel pool in
         let%lwt () = create_dead_letter channel pool in
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

  let to_temp_queue channel pool =
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
    let%lwt shovel_to_temp =
      Queue.declare
        ~durable:true
        ~arguments:[ "x-shovel", shovel_to_temp_payload ]
        channel
        shovel_to_temp_name
    in
    (* Wait for the Shovel to move all messages *)
    let%lwt () = Lwt_unix.sleep 5.0 in
    Lwt.return [ temp_queue; shovel_to_temp ]
  ;;

  let to_orig_queue channel pool =
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

  let cleanup_original channel pool =
    Lwt_list.iter_s
      (fun queue -> Queue.declare ~passive:true channel queue >|> Queue.delete channel)
      (QueueName.to_remove_after_migration pool)
  ;;

  let delete_queue channel = Lwt_list.iter_s (Queue.delete channel)

  let connect () =
    Connection.connect ~id:"email_service" ~credentials:C.credentials C.host
  ;;

  let open_channel connection =
    Connection.open_channel ~id:"email_channel" Channel.with_confirm connection
  ;;

  let init =
    Lwt_list.iter_s (fun pool ->
      let%lwt connection = connect () in
      let%lwt channel = open_channel connection in
      let%lwt queue =
        match%lwt catch_create channel pool with
        | Some queue -> Lwt.return queue
        | None ->
          let%lwt temp_queue = to_temp_queue channel pool in
          let%lwt () = cleanup_original channel pool in
          let%lwt queue = create_queue channel pool in
          let%lwt () = create_dead_letter channel pool in
          let%lwt shovel = to_orig_queue channel pool in
          let%lwt () = delete_queue channel (temp_queue @ shovel) in
          Lwt.return queue
      in
      let () = InstanceCache.add pool (Instance.create connection channel queue) in
      Lwt.return_unit)
  ;;

  let get_instance pool =
    InstanceCache.find_opt pool
    |> function
    | Some instance -> instance
    | None -> failwith "RabbitMQ instance is not initialized"
  ;;

  let dispatch pool ~message_id ~payload =
    let open Utils.Lwt_result.Infix in
    Logs.warn (fun m ->
      m "Dispatching email to RabbitMQ [%s]: %s" (Database.Label.value pool) message_id);
    Logs.warn (fun m -> m "Payload: %s" payload);
    Logs.warn (fun m ->
      m
        "%s"
        (InstanceCache.tbl
         |> Hashtbl.to_seq_keys
         |> Seq.map Database.Label.value
         |> CCList.of_seq
         |> CCString.concat ", "));
    Logs.warn (fun m ->
      m
        "Instance [%s]: %s"
        (Database.Label.value pool)
        (InstanceCache.find_opt pool
         |> CCOption.map (fun _ -> "found")
         |> CCOption.value ~default:"not found"));
    let { Instance.channel; queue; _ } = get_instance pool in
    let message = Message.make ~message_id payload in
    Queue.publish channel queue message
    >|> function
    | `Ok -> Lwt.return_unit
    | `Failed -> Lwt.fail_with "Failed to publish message to RabbitMQ queue"
  ;;

  let close () =
    Hashtbl.fold
      (fun _pool { Instance.channel; connection; _ } acc ->
         acc
         >|> fun () ->
         let%lwt () = Channel.close channel in
         let%lwt () = Connection.close connection in
         Lwt.return_unit)
      InstanceCache.tbl
      Lwt.return_unit
  ;;
end
