module Map = CCMap.Make (String)

let execute db_pools steps =
  Lwt_list.iter_s
    (fun db_pool ->
      Service.Migration.execute
        ~ctx:[ "pool", Pool_common.Database.Label.value db_pool ]
        steps)
    db_pools
;;

let extend_migrations additional_steps () =
  let registered_migrations =
    let open Sihl.Database.Migration in
    !registered_migrations
  in
  let migrations = (registered_migrations |> Map.to_list) @ additional_steps in
  match
    CCList.length migrations
    == CCList.length
         (CCList.uniq
            ~eq:(fun (k1, _) (k2, _) -> CCString.equal k1 k2)
            migrations)
  with
  | true -> migrations
  | false ->
    Logs.info (fun m ->
        m "There are duplicated migrations. Remove or rename them.");
    []
;;

let run_pending_migrations db_pools migration_steps =
  let open Pool_common.Database.Label in
  let%lwt status =
    Lwt_list.map_s
      (fun label ->
        let%lwt m =
          Service.Migration.pending_migrations ~ctx:[ "pool", value label ] ()
        in
        (label, m) |> Lwt.return)
      db_pools
  in
  Lwt_list.iter_s
    (fun (label, pending_migrations) ->
      let msg prefix =
        Format.asprintf "%s pending migration for database pool: %s" prefix
        @@ value label
      in
      if CCList.length pending_migrations > 0
      then (
        Logs.debug (fun m -> m "%s" @@ msg "Run");
        execute [ label ] migration_steps)
      else (
        Logs.debug (fun m -> m "%s" @@ msg "No");
        Lwt.return_unit))
    status
;;

module Root = struct
  let steps = extend_migrations [ Migration_tenant.migration () ]
  let run () = execute [ Pool_common.Database.root ] @@ steps ()

  let run_pending_migrations () =
    run_pending_migrations [ Pool_common.Database.root ] @@ steps ()
  ;;
end

module Tenant = struct
  let steps =
    extend_migrations
      [ Migration_person.migration ()
      ; Migration_participant.migration ()
      ; Migration_email.migration ()
      ]
  ;;

  let run db_pools () = execute db_pools @@ steps ()

  let run_pending_migrations db_pools () =
    run_pending_migrations db_pools @@ steps ()
  ;;
end
