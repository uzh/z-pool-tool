open CCFun
module RepoEntity = Repo_entity

module Sql = struct
  open Pool_database

  let select_sql =
    {sql|
      SELECT
        LOWER(CONCAT(
          SUBSTR(HEX(pool_system_events.uuid), 1, 8), '-',
          SUBSTR(HEX(pool_system_events.uuid), 9, 4), '-',
          SUBSTR(HEX(pool_system_events.uuid), 13, 4), '-',
          SUBSTR(HEX(pool_system_events.uuid), 17, 4), '-',
          SUBSTR(HEX(pool_system_events.uuid), 21)
        )),
        pool_system_events.job,
        pool_system_events.worker_only,
        pool_system_events.created_at,
        pool_system_events.updated_at
      FROM
        pool_system_events
    |sql}
  ;;

  let find_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE
        pool_system_events.uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> Format.asprintf "%s\n%s" select_sql
    |> RepoEntity.Id.t ->! RepoEntity.t
  ;;

  let find pool id =
    let open Utils.Lwt_result.Infix in
    Utils.Database.find_opt (Label.value pool) find_request id
    ||> CCOption.to_result Pool_common.Message.(NotFound Field.SystemEvent)
  ;;

  let insert_request =
    let open Caqti_request.Infix in
    {sql|
      INSERT INTO pool_system_events (
        uuid,
        job,
        worker_only,
        created_at,
        updated_at
      ) VALUES (
        UNHEX(REPLACE($1, '-', '')),
        $2,
        $3,
        $4,
        $5
      )
    |sql}
    |> RepoEntity.t ->. Caqti_type.unit
  ;;

  let insert = Label.value %> flip Utils.Database.exec insert_request

  module EventLog = struct
    let select_sql =
      {sql|
        SELECT
          LOWER(CONCAT(
            SUBSTR(HEX(logs.event_uuid), 1, 8), '-',
            SUBSTR(HEX(logs.event_uuid), 9, 4), '-',
            SUBSTR(HEX(logs.event_uuid), 13, 4), '-',
            SUBSTR(HEX(logs.event_uuid), 17, 4), '-',
            SUBSTR(HEX(logs.event_uuid), 21)
          )),
          logs.service_identifier,
          logs.status,
          logs.message,
          logs.created_at,
          logs.updated_at
        FROM
          pool_system_event_logs AS logs
      |sql}
    ;;

    (* Required? *)
    let find_by_event_and_host_request =
      let open Caqti_request.Infix in
      {sql|
        JOIN pool_system_events AS events ON logs.event_uuid = events.uuid
        WHERE
          events.uuid = UNHEX(REPLACE(?, '-', ''))
          AND logs.service_identifier = ?
        ORDER BY events.created_at DESC
      |sql}
      |> Format.asprintf "%s\n%s" select_sql
      |> Caqti_type.(t2 RepoEntity.Id.t RepoEntity.EventLog.ServiceIdentifier.t)
         ->* RepoEntity.t
    ;;

    let find_by_event_and_host pool =
      Utils.Database.collect
        (Pool_database.Label.value pool)
        find_by_event_and_host_request
    ;;

    let insert_request =
      let open Caqti_request.Infix in
      (* TODO: Consider using UPDATE ON DUPLICATE when event_id/service_id pair
         should be unique *)
      {sql|
        INSERT INTO pool_system_event_logs (
          event_uuid,
          service_identifier,
          status,
          message,
          created_at,
          updated_at
        ) VALUES (
          UNHEX(REPLACE($1, '-', '')),
          $2,
          $3,
          $4,
          $5,
          $6
        )
      |sql}
      |> RepoEntity.EventLog.t ->. Caqti_type.unit
    ;;

    let insert = Label.value %> flip Utils.Database.exec insert_request
  end

  let find_pending_request ~worker () =
    let open Caqti_request.Infix in
    let joins =
      {sql|
        LEFT JOIN pool_system_event_logs
          ON pool_system_event_logs.event_uuid = pool_system_events.uuid
            AND pool_system_event_logs.service_identifier = $1
      |sql}
    in
    let base_condition =
      {sql|
        pool_system_event_logs.status IS NULL
        OR pool_system_event_logs.status != "successful"
      |sql}
    in
    let exclude_worker_only =
      {sql|
        pool_system_events.worker_only = 0
      |sql}
    in
    (match worker with
     | true -> Format.asprintf "%s WHERE %s" joins base_condition
     | false ->
       Format.asprintf
         "%s WHERE %s AND (%s)"
         joins
         exclude_worker_only
         base_condition)
    |> Format.asprintf "%s\n%s" select_sql
    |> RepoEntity.EventLog.ServiceIdentifier.t ->! RepoEntity.t
  ;;

  let find_pending ?(worker = false) () =
    Entity.EventLog.ServiceIdentifier.get ~worker ()
    |> Utils.Database.collect
         (Label.value root)
         (find_pending_request ~worker ())
  ;;
end

let find = Sql.find
let insert = Sql.insert
let find_pending = Sql.find_pending

module EventLog = struct
  let find_by_event_and_host_request = Sql.EventLog.find_by_event_and_host
  let insert = Sql.EventLog.insert
end
