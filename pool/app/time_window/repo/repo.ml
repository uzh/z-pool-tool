open CCFun.Infix
module RepoEntity = Repo_entity
module Dynparam = Database.Dynparam

let sql_select_columns =
  [ Session.Id.sql_select_fragment ~field:"pool_sessions.uuid"
  ; "pool_sessions.start"
  ; "pool_sessions.duration"
  ; "pool_sessions.internal_description"
  ; "pool_sessions.public_description"
  ; "pool_sessions.max_participants"
  ; "COUNT(pool_assignments.id) as assignment_count"
  ; "COALESCE( SUM(pool_assignments.no_show), 0) as noshow_count"
  ; "COALESCE( SUM(pool_assignments.participated), 0) as participation_count"
  ; "pool_sessions.created_at"
  ; "pool_sessions.updated_at"
  ]
  @ Experiment.Repo.sql_select_columns
;;

let joins =
  Format.asprintf
    {sql|
      LEFT JOIN pool_assignments
        ON pool_assignments.session_uuid = pool_sessions.uuid
        AND pool_assignments.canceled_at IS NULL
        AND pool_assignments.marked_as_deleted = 0
      INNER JOIN pool_experiments
        ON pool_experiments.uuid = pool_sessions.experiment_uuid
      %s
    |sql}
    Experiment.Repo.joins
;;

let find_request_sql ?(count = false) where_fragment =
  let columns =
    if count then "1" else sql_select_columns |> CCString.concat ", "
  in
  let query =
    Format.asprintf
      {sql| SELECT %s FROM pool_sessions %s %s GROUP BY pool_sessions.uuid |sql}
      columns
      joins
      where_fragment
  in
  match count with
  | false -> query
  | true -> Format.asprintf "SELECT COUNT(*) FROM (%s) c" query
;;

let find_request =
  let open Caqti_request.Infix in
  {sql|
      WHERE pool_sessions.uuid = UNHEX(REPLACE(?, '-', ''))
  |sql}
  |> find_request_sql
  |> Caqti_type.string ->! RepoEntity.t
;;

let find pool id =
  let open Utils.Lwt_result.Infix in
  Database.find_opt pool find_request (Session.Id.value id)
  ||> CCOption.to_result Pool_message.(Error.NotFound Field.Session)
;;

let find_overlapping_request exclude =
  let end_at =
    "DATE_ADD(pool_sessions.start, INTERVAL pool_sessions.duration SECOND)"
  in
  let base =
    [%string
      {sql|
        WHERE
          pool_sessions.experiment_uuid = UNHEX(REPLACE($1, '-', ''))
          AND pool_sessions.start <= $3
          AND %{end_at} >= $2
      |sql}]
  in
  let where =
    if CCOption.is_some exclude
    then
      Format.asprintf
        "%s AND pool_sessions.uuid != UNHEX(REPLACE($4, '-', ''))"
        base
    else base
  in
  where |> find_request_sql
;;

let find_overlapping ?exclude pool experiment_id ~start ~end_at =
  let open Caqti_request.Infix in
  let open Session in
  let (Dynparam.Pack (pt, pv)) =
    let open Dynparam in
    empty
    |> add Experiment.Repo.Entity.Id.t experiment_id
    |> add Repo.Start.t start
    |> add Repo.End.t end_at
    |> fun dyn ->
    match exclude with
    | None -> dyn
    | Some id -> dyn |> add Repo.Id.t id
  in
  let request = find_overlapping_request exclude |> pt ->* RepoEntity.t in
  Database.collect pool request pv
;;

let insert_request =
  let open Caqti_request.Infix in
  {sql|
    INSERT INTO pool_sessions (
      uuid,
      experiment_uuid,
      start,
      duration,
      internal_description,
      public_description,
      max_participants
    ) VALUES (
      UNHEX(REPLACE($1, '-', '')),
      UNHEX(REPLACE($2, '-', '')),
      $3,
      $4,
      $5,
      $6,
      $7
    )
  |sql}
  |> Caqti_type.(RepoEntity.Write.t ->. unit)
;;

let insert pool m =
  Database.exec pool insert_request (RepoEntity.Write.of_entity m)
;;

let update_request =
  let open Caqti_request.Infix in
  {sql|
    UPDATE pool_sessions
    SET
      start = $3,
      duration = $4,
      internal_description = $5,
      public_description = $6,
      max_participants = $7
    WHERE
      uuid = UNHEX(REPLACE($1, '-', ''))
  |sql}
  |> RepoEntity.Write.t ->. Caqti_type.unit
;;

let update pool =
  RepoEntity.Write.of_entity %> Database.exec pool update_request
;;

let delete_request =
  let open Caqti_request.Infix in
  {sql|
      DELETE FROM pool_sessions
      WHERE uuid = UNHEX(REPLACE(?, '-', ''))
  |sql}
  |> Session.Repo.Id.t ->. Caqti_type.unit
;;

let delete pool time_window =
  Database.exec pool delete_request time_window.Entity.id
;;

let query_by_experiment ?query pool id =
  let where =
    let sql =
      {sql| pool_sessions.experiment_uuid = UNHEX(REPLACE(?, '-', '')) |sql}
    in
    let dyn = Dynparam.(empty |> add Experiment.Repo.Entity.Id.t id) in
    sql, dyn
  in
  Query.collect_and_count
    pool
    query
    ~select:find_request_sql
    ~where
    Repo_entity.t
;;

let find_by_experiment_and_time_request time =
  let open Caqti_request.Infix in
  let where, limit =
    match time with
    | `Current ->
      let where =
        {sql|
          pool_experiments.uuid = UNHEX(REPLACE($1, '-', ''))
          AND pool_sessions.start < NOW()
          AND DATE_ADD(pool_sessions.start, INTERVAL pool_sessions.duration SECOND) > NOW()
          AND pool_sessions.canceled_at IS NULL
        |sql}
      in
      where, ""
    | `Upcoming ->
      let where =
        {sql|
          pool_experiments.uuid = UNHEX(REPLACE($1, '-', ''))
          AND DATE_ADD(pool_sessions.start, INTERVAL pool_sessions.duration SECOND) > NOW()
          AND pool_sessions.canceled_at IS NULL
        |sql}
      in
      let limit =
        {sql|
          ORDER BY pool_sessions.start ASC
          LIMIT 1 
        |sql}
      in
      where, limit
  in
  Format.asprintf
    "SELECT %s FROM pool_sessions %s WHERE %s GROUP BY pool_sessions.uuid %s"
    (CCString.concat ", " sql_select_columns)
    joins
    where
    limit
  |> Pool_common.Repo.Id.t ->! RepoEntity.t
;;

let find_by_experiment_and_time time pool experiment_id =
  Database.find_opt
    pool
    (find_by_experiment_and_time_request time)
    (Experiment.Id.to_common experiment_id)
;;
