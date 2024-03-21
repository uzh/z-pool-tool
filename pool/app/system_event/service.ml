let src = Logs.Src.create "system_event.service"
let get_or_failwith = Pool_common.Utils.get_or_failwith

let run ?worker () =
  let () =
    Logs.info (fun m ->
      m
        "RUNNING SYSTEM EVENTS. Worker: %s"
        (Utils.Bool.to_string (CCOption.value ~default:false worker)))
  in
  let open Utils.Lwt_result.Infix in
  ()
  |> Repo.find_pending ?worker
  >|> Lwt_list.iter_s (Event.handle_system_event ?worker)
;;

let start_handler ?worker () =
  let open Schedule in
  let interval = Ptime.Span.of_int_s 10 in
  let periodic_fcn () =
    Logs.debug ~src (fun m ->
      m ~tags:Pool_database.(Logger.Tags.create root) "Run");
    run ?worker ()
  in
  create
    "system_events"
    (Every (interval |> ScheduledTimeSpan.of_span))
    periodic_fcn
  |> Schedule.add_and_start
;;

type env = Run

let to_string = function
  | Run -> "SYSTEM_EVENTS_RUN"
;;

type config = { start : bool option }

let config start = { start }

let read_variable fcn env =
  fcn (env |> to_string)
  |> CCOption.get_exn_or
       (Format.asprintf "Variable not defined: %s" (env |> to_string))
;;

let read_bool = read_variable Sihl.Configuration.read_bool

let schema =
  let open Conformist in
  make
    Field.
      [ Conformist.optional
          (bool
             ~meta:"If set to false, system events will not be handled."
             ~default:(Sihl.Configuration.is_production ())
             (Run |> to_string))
      ]
    config
;;

let start ?worker () =
  Sihl.Configuration.require schema;
  if read_bool Run then start_handler ?worker () else Lwt.return_unit
;;

let stop () = Lwt.return_unit

let lifecycle ?worker () =
  Sihl.Container.create_lifecycle
    "System events"
    ~dependencies:(fun () -> [ Schedule.lifecycle ])
    ~start:(start ?worker)
    ~stop
;;

let register ?worker () =
  let configuration = Sihl.Configuration.make ~schema () in
  Sihl.Container.Service.create ~configuration (lifecycle ?worker ())
;;

let register_worker = register ~worker:true
