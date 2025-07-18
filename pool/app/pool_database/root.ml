open Migrations

let sort = CCList.stable_sort (fun a b -> CCString.compare (fst a) (fst b))

let steps =
  let sorted =
    Migration_authorization.migration ()
    @ [ Migration_202301010000.migration ()
      ; Migration_202305261314.migration ()
      ; Migration_202306021033.migration ()
      ; Migration_202306261642.migration_root ()
      ; Migration_202307121722.migration_root ()
      ; Migration_202307131619.migration_root ()
      ; Migration_202309180820.migration_root ()
      ; Migration_202309211305.migration_root ()
      ; Migration_202309261013.migration_root ()
      ; Migration_202312200952.migration ()
      ; Migration_202401081554.migration ()
      ; Migration_202401221730.migration ()
      ; Migration_202402091434.migration ()
      ; Migration_202403201132.migration ()
      ; Migration_202404051544.migration ()
      ; Migration_202406051700.migration ()
      ; Migration_202406071359.migration ()
      ; Migration_202407171415.migration ()
      ; Migration_202408131047.migration ()
      ; Migration_202409021617.migration ()
      ; Migration_202410031211.migration ()
      ; Migration_202410181519.migration ()
      ; Migration_202504111642.migration ()
      ; Migration_202504141225.migration ()
      ; Migration_202505121331.migration ()
      ; Migration_202506261422.migration ()
      ]
    |> sort
  in
  [ Migration_tenant.migration_root ()
  ; Migration_tenant_logo_mappings.migration ()
  ; Migration_i18n.migration ()
  ; Migration_message_templates.migration ()
  ; Migration_schedule.migration ()
  ]
  @ sorted
  |> Database.Migration.extend_migrations
;;

let start () =
  let open Database in
  let migrations = steps () in
  let%lwt () = Migration.check_migrations_status Pool.Root.label ~migrations () in
  Lwt.return_unit
;;

let stop () = Lwt.return_unit
