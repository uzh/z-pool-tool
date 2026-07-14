open CCFun
include Entity
module Guard = Entity_guard

let src = Logs.Src.create "schedule.service"
let tags = Database.(Logger.Tags.create Pool.Root.label)

module Registered = struct
  module ScheduleMap = CCMap.Make (Label)

  type t = Entity.t

  let registered : t ScheduleMap.t ref = ref ScheduleMap.empty

  let iter_lwt fcn =
    !registered
    |> ScheduleMap.to_list
    |> Lwt_list.iter_s (fun (_, schedule) -> fcn schedule)
  ;;

  let find_opt label = ScheduleMap.find_opt label !registered
  let store label = find_opt label |> CCOption.map_or ~default:Lwt.return_unit Repo.upsert

  let store_all () =
    !registered |> ScheduleMap.to_list |> Lwt_list.iter_s (snd %> Repo.upsert)
  ;;

  let add_base ({ label; _ } as schedule : t) =
    Logs.debug ~src (fun m -> m ~tags "registered '%s'" label);
    registered := ScheduleMap.add label schedule !registered
  ;;

  let add ({ label; _ } as schedule : t) =
    add_base schedule;
    store label
  ;;

  let update_status status ({ label; _ } : t) =
    let update_status = CCOption.map (fun (s : t) -> { s with status }) in
    registered := ScheduleMap.update label update_status !registered;
    store label
  ;;

  let update_last_run_base ({ label; _ } : t) =
    let last_run = Some (LastRunAt.create_now ()) in
    let add_last_run = CCOption.map (fun (s : t) -> { s with last_run }) in
    registered := ScheduleMap.update label add_last_run !registered
  ;;

  let update_last_run ({ label; _ } as schedule : t) =
    update_last_run_base schedule;
    store label
  ;;

  let finish_base ({ label; _ } : t) =
    let open ScheduleMap in
    let add_finished =
      CCOption.map (fun schedule : Entity.t ->
        let last_run = Some (LastRunAt.create_now ()) in
        let status = Status.Finished in
        { schedule with last_run; status })
    in
    registered := update label add_finished !registered
  ;;

  let finish ({ label; _ } as schedule : t) =
    let () = finish_base schedule in
    let%lwt () = store label in
    let () = registered := ScheduleMap.remove label !registered in
    Lwt.return_unit
  ;;

  let update_run_status schedule = function
    | Every _ -> update_last_run schedule
    | At _ -> finish schedule
  ;;

  let stop_all_active () =
    let open ScheduleMap in
    let stop_if_active ({ status; _ } as schedule : Entity.t) =
      if Status.(equal Active) status
      then { schedule with status = Status.Stopped }
      else schedule
    in
    registered := map stop_if_active !registered;
    store_all ()
  ;;
end

let run ({ database_label; label; scheduled_time; status; _ } as schedule : t) =
  let open Utils.Lwt_result.Infix in
  let tags = CCOption.map_or ~default:tags Database.Logger.Tags.create database_label in
  let delay = run_in scheduled_time in
  let notify status =
    Logs.debug ~src (fun m -> m ~tags "%s: Run is %s" label (Status.show status));
    Lwt.return_unit
  in
  (* Bookkeeping writes go to the root database. When it is unreachable they must
     not kill the schedule loop: log the exception and continue. *)
  let safely action fcn =
    Lwt.catch fcn (fun exn ->
      let prefix = Format.asprintf "Schedule %s: %s" label action in
      Logger.log_exception ~prefix ~tags ~src exn;
      Lwt.return_unit)
  in
  let run ({ label; scheduled_time; fcn; _ } as schedule) =
    Logs.debug ~src (fun m -> m ~tags "%s: Run in %f seconds" label delay);
    let%lwt succeeded =
      Lwt.catch
        (fun () -> fcn () ||> CCFun.const true)
        (fun exn ->
           let prefix = Format.asprintf "Running schedule %s" label in
           Logger.log_exception ~prefix ~tags ~src exn;
           let%lwt () =
             safely "store failed status" (fun () ->
               Registered.update_status Status.Failed schedule)
           in
           Lwt.return_false)
    in
    let%lwt () =
      let previously_marked_failed =
        Registered.find_opt label
        |> CCOption.map_or ~default:false (fun ({ status; _ } : t) ->
          Status.(equal Failed status))
      in
      if succeeded && previously_marked_failed
      then
        safely "restore active status" (fun () ->
          Registered.update_status Status.Active schedule)
      else Lwt.return_unit
    in
    safely "store run status" (fun () ->
      Registered.update_run_status schedule scheduled_time)
  in
  let rec loop () : unit Lwt.t =
    let database_ok () =
      let open Database in
      let open Status in
      let database_status =
        CCOption.map_or ~default:Active (fun label ->
          match Pool.Tenant.find label with
          | Ok database -> status database
          | Error _ -> Disabled)
      in
      let retry_connection label =
        (* Reset database connection to "Close" before retry *)
        let%lwt () = Pool.disconnect label in
        Pool.connect label
        >|> function
        | Ok () ->
          let%lwt () =
            let open Pool_database in
            StatusUpdated (label, Active) |> handle_event Pool.Root.label
          in
          Lwt.return_true
        | Error _ -> Lwt.return_false
      in
      match database_status database_label with
      | Active -> Lwt.return_true
      | ConnectionIssue ->
        CCOption.map_or ~default:Lwt.return_false retry_connection schedule.database_label
      | Disabled
      | Maintenance
      | MigrationsConnectionIssue
      | MigrationsFailed
      | MigrationsPending -> Lwt.return false
    in
    let run_schedule () =
      let process schedule =
        let%lwt () = run schedule in
        Lwt.return `Rerun
      in
      let open Status in
      match scheduled_time, status with
      | Every _, Active -> process schedule
      | At time, Active when Ptime.is_later ~than:(Utils.Ptime.now ()) time ->
        Lwt.return `Rerun
      | At _, Active ->
        let%lwt () =
          safely "store running status" (fun () ->
            Registered.update_status Status.Running schedule)
        in
        let%lwt () = run schedule in
        Lwt.return `Stop
      | (Every _ | At _), ((Paused | Failed) as status) ->
        let%lwt () = notify status in
        Lwt.return `Stop
      | (Every _ | At _), (Finished | Running | Stopped) -> Lwt.return `Stop
    in
    (* Catch per iteration (not around the recursion) so that an unexpected
       exception - e.g. an unreachable database - never terminates the loop. *)
    let%lwt next =
      Lwt.catch
        (fun () ->
           match%lwt database_ok () with
           | true -> run_schedule ()
           | false -> Lwt.return `Rerun)
        (fun exn ->
           let prefix = Format.asprintf "Schedule %s: loop iteration" label in
           Logger.log_exception ~prefix ~tags ~src exn;
           Lwt.return `Rerun)
    in
    match next with
    | `Rerun -> Lwt_unix.sleep (run_in scheduled_time) >|> loop
    | `Stop -> Lwt.return_unit
  in
  loop ()
;;

let start_schedule ?tags ({ label; _ } as schedule : t) =
  Logs.info ~src (fun m -> m ?tags "Start %s" label);
  Lwt.ignore_result @@ run schedule;
  Lwt.return_unit
;;

let start ?tags () =
  let%lwt () = Repo.stop_all_active () in
  Logs.info ~src (fun m -> m ?tags "Start schedule");
  Registered.iter_lwt (start_schedule ?tags)
;;

let stop = Registered.stop_all_active

let lifecycle =
  Sihl.Container.create_lifecycle
    "pool schedule"
    ~dependencies:(fun () -> [ Pool_database.lifecycle ])
    ~start
    ~stop
;;

let register ?(schedules = []) () =
  CCList.iter Registered.add_base schedules;
  Sihl.Container.Service.create lifecycle
;;

let add_and_start ?tags schedule =
  let%lwt () = Registered.add schedule in
  start_schedule ?tags schedule
;;

let find_all = Repo.find_all
let find_by_db_label = Repo.find_by_db_label
