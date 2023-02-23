include Entity
module ScheduleMap = CCMap.Make (Label)

let log_src = Logs.Src.create "pool.service.schedule"

module Logs = (val Logs.src_log log_src : Logs.LOG)

module RegisteredSchedules : sig
  type t = Entity.t

  val find_all : unit -> t list
  val iter : (t -> unit) -> unit
  val add : t -> unit Lwt.t
  val add_multiple : t list -> unit Lwt.t
  val update_last_run : t -> unit Lwt.t
  val finish : t -> unit Lwt.t
  val update_run_status : scheduled_time -> t -> unit Lwt.t
  val stop_all_active : unit -> unit Lwt.t
end = struct
  type t = Entity.t

  let registered : t ScheduleMap.t ref = ref ScheduleMap.empty
  let iter fcn = ScheduleMap.iter (fun _ schedule -> fcn schedule) !registered
  let store label = ScheduleMap.find label !registered |> Repo.upsert

  let store_all () =
    !registered
    |> ScheduleMap.to_list
    |> Lwt_list.iter_s CCFun.(snd %> Repo.upsert)
  ;;

  let add ({ label; _ } as schedule) =
    registered := ScheduleMap.add label schedule !registered;
    store label
  ;;

  let add_multiple = Lwt_list.iter_s add

  let update_last_run_base { label; _ } =
    let last_run = Some (LastRun.create_now ()) in
    let add_last_run = CCOption.map (fun s -> { s with last_run }) in
    registered := ScheduleMap.update label add_last_run !registered
  ;;

  let update_last_run ({ label; _ } as schedule) =
    update_last_run_base schedule;
    store label
  ;;

  let finish_base { label; _ } =
    let open ScheduleMap in
    let add_finished =
      CCOption.map (fun schedule ->
        let last_run = Some (LastRun.create_now ()) in
        let status = Status.Finished in
        { schedule with last_run; status })
    in
    registered := update label add_finished !registered
  ;;

  let finish ({ label; _ } as schedule) =
    finish_base schedule;
    store label
  ;;

  let update_run_status scheduled_time ({ label; _ } as schedule) =
    (match scheduled_time with
     | Every _ -> update_last_run_base
     | At _ -> finish_base)
      schedule;
    store label
  ;;

  let stop_all_active () =
    let open ScheduleMap in
    let stop_if_active ({ status; _ } as schedule) =
      if Status.(equal Active) status
      then { schedule with status = Status.Stopped }
      else schedule
    in
    registered := map stop_if_active !registered;
    store_all ()
  ;;

  let find_all () = !registered |> ScheduleMap.to_list |> CCList.map snd
end

let run ?now ({ label; scheduled_time; status; fcn; _ } as schedule) =
  let should_stop = ref false in
  let stop_schedule () = should_stop := true in
  Logs.info (fun m -> m "Schedule %s" label);
  let delay = run_in ?now schedule in
  let paused () =
    Logs.debug (fun m -> m "Schedule %s: Run is paused" label);
    Lwt.return_unit
  in
  let run () =
    Logs.debug (fun m -> m "Schedule %s: Run in %f seconds" label delay);
    let%lwt () =
      Lwt.catch
        (fun () -> fcn ())
        (fun exn ->
          Logs.err (fun m ->
            m
              "Exception caught while running schedule, this is a bug in your \
               scheduled function.\n\
              \ %s"
              (Printexc.to_string exn));
          Lwt.return_unit)
    in
    RegisteredSchedules.update_run_status scheduled_time schedule
  in
  let rec loop () =
    let%lwt () =
      let open Status in
      match scheduled_time, status with
      | Every _, Active -> run ()
      | At time, Active when Ptime.is_later ~than:(Ptime_clock.now ()) time ->
        run ()
      | (Every _ | At _), Paused -> paused ()
      | (Every _ | At _), (Active | Finished | Stopped) -> Lwt.return_unit
    in
    let%lwt () = Lwt_unix.sleep delay in
    if !should_stop
    then (
      let () = Logs.debug (fun m -> m "Schedule %s: Stopp" label) in
      Lwt.return ())
    else loop ()
  in
  loop () |> ignore;
  stop_schedule ()
;;

let start _ =
  let open RegisteredSchedules in
  let registered = find_all () |> CCList.map (fun { label; _ } -> label) in
  Logs.info (fun m ->
    m "Schedulers registered: %s" ([%show: Label.t list] registered));
  iter run;
  Lwt.return_unit
;;

let stop _ = RegisteredSchedules.stop_all_active ()
let lifecycle = Sihl.Container.create_lifecycle "schedule" ~start ~stop

let register schedules =
  let%lwt () = RegisteredSchedules.add_multiple schedules in
  Sihl.Container.Service.create lifecycle |> Lwt.return
;;
