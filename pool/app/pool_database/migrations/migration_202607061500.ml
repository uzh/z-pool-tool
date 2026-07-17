(* One-off timezone normalisation (report.md §3/§6).

   Until this release, database sessions ran in the server's Europe/Zurich time
   zone and the application host's clock follows Zurich wall time, so every
   DATETIME column holds Zurich wall-clock — regardless of whether it was
   written by a SQL default, NOW() or the application. From this release on,
   sessions are pinned to UTC on connect (pool/database/pools.ml), so existing
   DATETIME values are converted to UTC exactly once here.

   TIMESTAMP columns are epoch-based (MariaDB converts them per session time
   zone on read and write) and stay consistent across the switch — they must
   NOT be converted.

   CONVERT_TZ with named zones is unavailable in production (the MariaDB
   mysql.time_zone* tables are not loaded), so the conversion function
   implements the EU DST rule directly: CEST between the last Sunday of March
   and the last Sunday of October, transition at 03:00 local wall time; CET
   otherwise. Ambiguous wall times in the repeated autumn hour resolve as CEST.

   updated_at is set in the same statement as the other columns on purpose:
   an explicit assignment prevents ON UPDATE CURRENT_TIMESTAMP from
   re-stamping it. *)

let function_name = "POOL_TMP_ZURICH_TO_UTC"

let create_conversion_function =
  [%string
    {sql|
      CREATE OR REPLACE FUNCTION %{function_name}(local_time DATETIME)
      RETURNS DATETIME
      DETERMINISTIC
      BEGIN
        DECLARE march_change DATETIME;
        DECLARE october_change DATETIME;
        IF local_time IS NULL OR local_time < '1000-01-01 00:00:00' THEN
          RETURN local_time;
        END IF;
        SET march_change = ADDTIME(
          DATE_SUB(
            LAST_DAY(MAKEDATE(YEAR(local_time), 70)),
            INTERVAL DAYOFWEEK(LAST_DAY(MAKEDATE(YEAR(local_time), 70))) - 1 DAY),
          '03:00:00');
        SET october_change = ADDTIME(
          DATE_SUB(
            LAST_DAY(MAKEDATE(YEAR(local_time), 280)),
            INTERVAL DAYOFWEEK(LAST_DAY(MAKEDATE(YEAR(local_time), 280))) - 1 DAY),
          '03:00:00');
        RETURN DATE_SUB(
          local_time,
          INTERVAL IF(local_time >= march_change AND local_time < october_change, 2, 1) HOUR);
      END
    |sql}]
  |> Database.Migration.Step.create ~label:"define zurich to utc conversion function"
;;

let drop_conversion_function =
  [%string {sql|DROP FUNCTION IF EXISTS %{function_name}|sql}]
  |> Database.Migration.Step.create ~label:"drop zurich to utc conversion function"
;;

let convert_table (table, columns) =
  let assignments =
    columns
    |> CCList.map (fun column ->
      [%string {sql|%{column} = %{function_name}(%{column})|sql}])
    |> CCString.concat ", "
  in
  [%string {sql|UPDATE %{table} SET %{assignments}|sql}]
  |> Database.Migration.Step.create ~label:[%string "convert %{table} to utc"]
;;

let audit_columns = [ "created_at"; "updated_at" ]

let queue_columns =
  [ "run_at"; "persisted_at"; "polled_at"; "handled_at"; "last_error_at" ] @ audit_columns
;;

(* All DATETIME columns present in both the root and tenant schemas. *)
let shared_tables =
  [ "guardian_actors", "mark_as_deleted" :: audit_columns
  ; "guardian_actor_permissions", "mark_as_deleted" :: audit_columns
  ; "guardian_actor_roles", "mark_as_deleted" :: audit_columns
  ; "guardian_actor_role_targets", "mark_as_deleted" :: audit_columns
  ; "guardian_assign_roles", audit_columns
  ; "guardian_assign_roles_history", audit_columns
  ; "guardian_role_permissions", "mark_as_deleted" :: audit_columns
  ; "guardian_targets", "mark_as_deleted" :: audit_columns
  ; "pool_queue_jobs", queue_columns
  ; "pool_queue_jobs_history", queue_columns
  ; "pool_queue_jobs_mapping", audit_columns
  ]
;;

(* DATETIME columns that only exist in tenant schemas. *)
let tenant_tables =
  [ "pool_cell_phone_verifications", [ "expires_at" ]
  ; "pool_contacts", [ "cell_phone_verified_at" ]
  ; "pool_experiments", [ "invitation_reset_at" ]
  ; "pool_mailing_invitations", audit_columns
  ]
;;

let steps tables =
  let open Database.Migration in
  fun migration ->
    migration
    |> add_step create_conversion_function
    |> CCFun.flip (CCList.fold_left (fun m t -> add_step (convert_table t) m)) tables
    |> add_step drop_conversion_function
;;

let migration () =
  Database.Migration.(empty "202607061500" |> steps (shared_tables @ tenant_tables))
;;

let migration_root () = Database.Migration.(empty "202607061500" |> steps shared_tables)
