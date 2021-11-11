module Root = struct
  let create () =
    let%lwt () = Seed_tenant.create () in
    Lwt.return_unit
  ;;
end

module Tenant = struct
  let create db_pools () =
    Lwt_list.iter_s
      (fun db_pool ->
        Logs.info (fun m -> m "%s" (Pool_common.Database.Label.value db_pool));
        let%lwt () = Seed_user.admins db_pool () in
        let%lwt () = Seed_user.participants db_pool () in
        let%lwt () = Seed_settings.create db_pool () in
        Lwt.return_unit)
      db_pools
  ;;
end
