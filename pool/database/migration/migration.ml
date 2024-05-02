module Database = Pool_database
module Map = CCMap.Make (String)

let src = Logs.Src.create "database.migration"
let sort = CCList.stable_sort (fun a b -> CCString.compare (fst a) (fst b))

let execute db_pools steps =
  Lwt_list.iter_s
    (fun pool ->
      Service.Migration.execute ~ctx:(Pool_database.to_ctx pool) steps)
    db_pools
;;

let extend_migrations additional_steps () =
  let registered_migrations =
    let open Sihl.Database.Migration in
    !registered_migrations
  in
  let migrations = (registered_migrations |> Map.to_list) @ additional_steps in
  match
    CCList.(length migrations == length (uniq ~eq:Utils.equal_key migrations))
  with
  | true -> migrations
  | false ->
    Logs.info ~src (fun m ->
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
      let tags = Pool_database.Logger.Tags.create label in
      let msg prefix =
        Format.asprintf "%s pending migration for database pool: %s" prefix
        @@ value label
      in
      if CCList.length pending_migrations > 0
      then (
        Logs.debug ~src (fun m -> m ~tags "%s" @@ msg "Run");
        execute [ label ] migration_steps)
      else (
        Logs.debug ~src (fun m -> m ~tags "%s" @@ msg "No");
        Lwt.return_unit))
    status
;;

module Root = struct
  let steps =
    let sorted =
      Migration_authorization.migration ()
      @ [ Migration_202305261314.migration ()
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
        ; Migration_202403201132.migration ()
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
    |> extend_migrations
  ;;

  let run () = execute [ Database.root ] @@ steps ()

  let run_pending_migrations () =
    run_pending_migrations [ Database.root ] @@ steps ()
  ;;
end

module Tenant = struct
  let steps =
    let sorted =
      Migration_authorization.migration ()
      @ [ Migration_202303211734.migration ()
        ; Migration_202303230956.migration ()
        ; Migration_202303291025.migration ()
        ; Migration_202305151556.migration ()
        ; Migration_202306021512.migration ()
        ; Migration_202306071352.migration ()
        ; Migration_202306081615.migration ()
        ; Migration_202306261642.migration ()
        ; Migration_202307081414.migration ()
        ; Migration_202307121006.migration ()
        ; Migration_202307121330.migration ()
        ; Migration_202307121722.migration ()
        ; Migration_202307131619.migration ()
        ; Migration_202307250839.migration ()
        ; Migration_202307281144.migration ()
        ; Migration_202308021016.migration ()
        ; Migration_202308030850.migration ()
        ; Migration_202308041330.migration ()
        ; Migration_202308210946.migration ()
        ; Migration_202309081700.migration ()
        ; Migration_202309121645.migration ()
        ; Migration_202309180820.migration ()
        ; Migration_202309190858.migration ()
        ; Migration_202309211305.migration ()
        ; Migration_202309251059.migration ()
        ; Migration_202309261013.migration ()
        ; Migration_202309281435.migration ()
        ; Migration_202310110929.migration ()
        ; Migration_202310191345.migration ()
        ; Migration_202311011626.migration ()
        ; Migration_202312040852.migration ()
        ; Migration_202312121047.migration ()
        ; Migration_202312150920.migration ()
        ; Migration_202312200952.migration ()
        ; Migration_202312201325.migration ()
        ; Migration_202401081554.migration ()
        ; Migration_202401120946.migration ()
        ; Migration_202401191048.migration ()
        ; Migration_202401221730.migration ()
        ; Migration_202402081615.migration ()
        ; Migration_202402091434.migration ()
        ; Migration_202402150942.migration ()
        ; Migration_202402161523.migration ()
        ; Migration_202402201330.migration ()
        ; Migration_202403131128.migration ()
        ; Migration_202403181624.migration ()
        ; Migration_202403221034.migration ()
        ; Migration_202403251311.migration ()
        ; Migration_202403281032.migration ()
        ; Migration_202403281435.migration ()
        ; Migration_202404101112.migration ()
        ]
      |> sort
    in
    [ Migration_person.migration ()
    ; Migration_contact.migration ()
    ; Migration_email_address.migration ()
    ; Migration_settings.migration ()
    ; Migration_i18n.migration ()
    ; Migration_assignment.migration ()
    ; Migration_session.migration ()
    ; Migration_invitation.migration ()
    ; Migration_experiment.migration ()
    ; Migration_waiting_list.migration ()
    ; Migration_location.migration ()
    ; Migration_location_file_mapping.migration ()
    ; Migration_mailing.migration ()
    ; Migration_filter.migration ()
    ; Migration_custom_fields.migration ()
    ; Migration_custom_field_answers.migration ()
    ; Migration_custom_field_options.migration ()
    ; Migration_custom_field_groups.migration ()
    ; Migration_custom_field_answer_versions.migration ()
    ; Migration_message_templates.migration ()
    ; Migration_tenant.migration_tenant ()
    ]
    @ sorted
    |> extend_migrations
  ;;

  let run db_pools () = execute db_pools @@ steps ()

  let run_pending_migrations db_pools () =
    run_pending_migrations db_pools @@ steps ()
  ;;
end
