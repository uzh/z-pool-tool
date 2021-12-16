module Database = Pool_database
module Map = CCMap.Make (String)

let execute db_pools steps =
  Lwt_list.iter_s
    (fun pool -> Service.Migration.execute ~ctx:(Tenant_pool.to_ctx pool) steps)
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
        m
          "There are duplicated migrations: %s\nRemove or rename them."
          (CCList.fold_left
             (fun a b -> Format.asprintf "%s\n%s" a (fst b))
             ""
             (CCList.stable_sort
                (fun a b -> CCString.compare (fst a) (fst b))
                migrations)));
    []
;;

let run_pending_migrations db_pools migration_steps =
  let open Database.Label in
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
  let steps =
    extend_migrations
      [ Migration_tenant.migration ()
      ; Migration_tenant_logo_mappings.migration ()
      ; Migration_email_templates.migration_root ()
      ]
  ;;

  let run () = execute [ Database.root ] @@ steps ()

  let run_pending_migrations () =
    run_pending_migrations [ Database.root ] @@ steps ()
  ;;
end

module Tenant = struct
  let steps =
    extend_migrations
      [ Migration_person.migration ()
      ; Migration_participant.migration ()
      ; Migration_email.migration ()
      ; Migration_email_templates.migration_tenant ()
      ; Migration_settings.migration ()
      ; Migration_email_templates.migration_signup ()
      ]
  ;;

  let run db_pools () = execute db_pools @@ steps ()

  let run_pending_migrations db_pools () =
    run_pending_migrations db_pools @@ steps ()
  ;;
end
