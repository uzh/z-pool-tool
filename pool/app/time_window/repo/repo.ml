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

let find_overlapping_request =
  let open Caqti_request.Infix in
  let end_at =
    "DATE_ADD(pool_sessions.start, INTERVAL pool_sessions.duration SECOND)"
  in
  (* TODO: How to deal with start or and at the same time? *)
  [%string
    {sql|
      WHERE
        pool_sessions.experiment_uuid = UNHEX(REPLACE($1, '-', ''))
        AND pool_sessions.start <= $3 
        AND %{end_at} >= $2
    |sql}]
  |> find_request_sql
  |> Caqti_type.(t3 string ptime ptime) ->* RepoEntity.t
;;

let find_overlapping pool experiment_id ~start ~end_at =
  let open Session in
  Utils.Database.collect
    (Pool_database.Label.value pool)
    find_overlapping_request
    (Experiment.Id.value experiment_id, Start.value start, End.value end_at)
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

let insert pool t =
  Utils.Database.exec
    (Pool_database.Label.value pool)
    insert_request
    (RepoEntity.Write.of_entity t)
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
