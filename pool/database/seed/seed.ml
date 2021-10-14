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
        Logs.info (fun m ->
            m
              "No seeds added for '%s' yet."
              (Pool_common.Database.Label.value db_pool));
        Lwt.return_unit)
      db_pools
  ;;
end
