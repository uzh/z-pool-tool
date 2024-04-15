module Database = Pool_database
module RepoEntity = Repo_entity
module Dynparam = Utils.Database.Dynparam

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
  ; "pool_sessions.closed_at"
  ; "pool_sessions.canceled_at"
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
  Utils.Database.find_opt
    (Pool_database.Label.value pool)
    find_request
    (Session.Id.value id)
  ||> CCOption.to_result Pool_common.Message.(NotFound Field.Session)
;;

let find_overlapping_request exclude =
  let end_at =
    "DATE_ADD(pool_sessions.start, INTERVAL pool_sessions.duration SECOND)"
  in
  (* TODO: How to deal with start or and at the same time? *)
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
    |> add Caqti_type.string (Experiment.Id.value experiment_id)
    |> add Caqti_type.ptime (Start.value start)
    |> add Caqti_type.ptime (End.value end_at)
    |> fun dyn ->
    match exclude with
    | None -> dyn
    | Some id -> dyn |> add Caqti_type.string (Id.value id)
  in
  let request = find_overlapping_request exclude |> pt ->* RepoEntity.t in
  Utils.Database.collect (pool |> Pool_database.Label.value) request pv
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
      max_participants,
      closed_at,
      canceled_at
    ) VALUES (
      UNHEX(REPLACE($1, '-', '')),
      UNHEX(REPLACE($2, '-', '')),
      $3,
      $4,
      $5,
      $6,
      $7,
      $8,
      $9
    )
  |sql}
  |> Caqti_type.(RepoEntity.Write.t ->. unit)
;;

let insert pool m =
  Utils.Database.exec
    (Pool_database.Label.value pool)
    insert_request
    (RepoEntity.Write.of_entity m)
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
      max_participants = $7,
      closed_at = $8,
      canceled_at = $9
    WHERE
      uuid = UNHEX(REPLACE($1, '-', ''))
  |sql}
  |> RepoEntity.Write.t ->. Caqti_type.unit
;;

let update pool m =
  Utils.Database.exec
    (Database.Label.value pool)
    update_request
    (RepoEntity.Write.of_entity m)
;;

let query_by_experiment ?query pool id =
  let where =
    let sql =
      {sql| pool_sessions.experiment_uuid = UNHEX(REPLACE(?, '-', '')) |sql}
    in
    let dyn =
      Dynparam.(empty |> add Pool_common.Repo.Id.t (Experiment.Id.to_common id))
    in
    sql, dyn
  in
  Query.collect_and_count
    pool
    query
    ~select:find_request_sql
    ~where
    Repo_entity.t
;;
