open CCFun
module Database = Pool_database
module Dynparam = Utils.Database.Dynparam

module Sql = struct
  let default_order_by = "pool_experiments.created_at"

  let insert_sql =
    {sql|
      INSERT INTO pool_experiments (
        uuid,
        title,
        public_title,
        description,
        cost_center,
        organisational_unit_uuid,
        filter_uuid,
        direct_registration_disabled,
        registration_disabled,
        allow_uninvited_signup,
        experiment_type,
        session_reminder_lead_time
      ) VALUES (
        UNHEX(REPLACE(?, '-', '')),
        ?,
        ?,
        ?,
        ?,
        UNHEX(REPLACE(?, '-', '')),
        UNHEX(REPLACE(?, '-', '')),
        ?,
        ?,
        ?,
        ?,
        ?
      )
    |sql}
  ;;

  let insert_request =
    let open Caqti_request.Infix in
    insert_sql |> Repo_entity.Write.t ->. Caqti_type.unit
  ;;

  let insert pool =
    Utils.Database.exec (Database.Label.value pool) insert_request
  ;;

  let select_from_experiments_sql where_fragment =
    let select_from =
      {sql|
        SELECT
          LOWER(CONCAT(
            SUBSTR(HEX(pool_experiments.uuid), 1, 8), '-',
            SUBSTR(HEX(pool_experiments.uuid), 9, 4), '-',
            SUBSTR(HEX(pool_experiments.uuid), 13, 4), '-',
            SUBSTR(HEX(pool_experiments.uuid), 17, 4), '-',
            SUBSTR(HEX(pool_experiments.uuid), 21)
          )),
          pool_experiments.title,
          pool_experiments.public_title,
          pool_experiments.description,
          pool_experiments.cost_center,
          LOWER(CONCAT(
            SUBSTR(HEX(pool_organisational_units.uuid), 1, 8), '-',
            SUBSTR(HEX(pool_organisational_units.uuid), 9, 4), '-',
            SUBSTR(HEX(pool_organisational_units.uuid), 13, 4), '-',
            SUBSTR(HEX(pool_organisational_units.uuid), 17, 4), '-',
            SUBSTR(HEX(pool_organisational_units.uuid), 21)
          )),
          pool_organisational_units.name,
          LOWER(CONCAT(
            SUBSTR(HEX(pool_filter.uuid), 1, 8), '-',
            SUBSTR(HEX(pool_filter.uuid), 9, 4), '-',
            SUBSTR(HEX(pool_filter.uuid), 13, 4), '-',
            SUBSTR(HEX(pool_filter.uuid), 17, 4), '-',
            SUBSTR(HEX(pool_filter.uuid), 21)
          )),
          pool_filter.query,
          pool_filter.title,
          pool_filter.created_at,
          pool_filter.updated_at,
          pool_experiments.direct_registration_disabled,
          pool_experiments.registration_disabled,
          pool_experiments.allow_uninvited_signup,
          pool_experiments.experiment_type,
          pool_experiments.session_reminder_lead_time,
          pool_experiments.created_at,
          pool_experiments.updated_at
        FROM pool_experiments
        LEFT JOIN pool_filter
          ON pool_filter.uuid = pool_experiments.filter_uuid
        LEFT JOIN pool_organisational_units
          ON pool_organisational_units.uuid = pool_experiments.organisational_unit_uuid
      |sql}
    in
    Format.asprintf "%s %s" select_from where_fragment
  ;;

  let validate_experiment_sql actor action =
    let open Guard.Persistence in
    let open Dynparam in
    ( {sql| guardianValidateExperimentUuid(guardianEncodeUuid(?), ?, pool_experiments.uuid) |sql}
    , empty |> add Uuid.Actor.t (actor |> Guard.Actor.id) |> add Action.t action
    )
  ;;

  let select_count where_fragment =
    Format.asprintf
      {sql|
        SELECT COUNT(*)
        FROM pool_experiments
        %s
      |sql}
      where_fragment
  ;;

  let find_all ?query ?actor ?action pool =
    let where =
      let open CCOption in
      ( and* ) actor action |> map (CCFun.uncurry validate_experiment_sql)
    in
    Query.collect_and_count
      pool
      query
      ~select:select_from_experiments_sql
      ~count:select_count
      ?where
      Repo_entity.t
  ;;

  let find_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE pool_experiments.uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> select_from_experiments_sql
    |> Caqti_type.string ->! Repo_entity.t
  ;;

  let find pool id =
    let open Utils.Lwt_result.Infix in
    Utils.Database.find_opt
      (Pool_database.Label.value pool)
      find_request
      (id |> Entity.Id.value)
    ||> CCOption.to_result Pool_common.Message.(NotFound Field.Experiment)
  ;;

  let find_of_session =
    let open Caqti_request.Infix in
    {sql|
      INNER JOIN pool_sessions
        ON pool_experiments.uuid = pool_sessions.experiment_uuid
      WHERE pool_sessions.uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> select_from_experiments_sql
    |> Caqti_type.string ->! Repo_entity.t
  ;;

  let find_of_session pool id =
    let open Utils.Lwt_result.Infix in
    Utils.Database.find_opt
      (Pool_database.Label.value pool)
      find_of_session
      (id |> Pool_common.Id.value)
    ||> CCOption.to_result Pool_common.Message.(NotFound Field.Experiment)
  ;;

  let find_of_mailing =
    let open Caqti_request.Infix in
    {sql|
      WHERE pool_experiments.uuid = (SELECT experiment_uuid FROM pool_mailing WHERE uuid = UNHEX(REPLACE(?, '-', '')) )
    |sql}
    |> select_from_experiments_sql
    |> Caqti_type.string ->! Repo_entity.t
  ;;

  let find_of_mailing pool id =
    let open Utils.Lwt_result.Infix in
    Utils.Database.find_opt
      (Pool_database.Label.value pool)
      find_of_mailing
      (id |> Pool_common.Id.value)
    ||> CCOption.to_result Pool_common.Message.(NotFound Field.Experiment)
  ;;

  let session_count_request =
    let open Caqti_request.Infix in
    {sql|
      SELECT COUNT(1) FROM pool_sessions WHERE experiment_uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> Caqti_type.(string ->! int)
  ;;

  let session_count pool id =
    Utils.Database.find
      (Pool_database.Label.value pool)
      session_count_request
      (id |> Pool_common.Id.value)
  ;;

  let update_request =
    let open Caqti_request.Infix in
    {sql|
      UPDATE pool_experiments
      SET
        title = $2,
        public_title = $3,
        description = $4,
        cost_center = $5,
        organisational_unit_uuid = UNHEX(REPLACE($6, '-', '')),
        filter_uuid = UNHEX(REPLACE($7, '-', '')),
        direct_registration_disabled = $8,
        registration_disabled = $9,
        allow_uninvited_signup = $10,
        experiment_type = $11,
        session_reminder_lead_time = $12
      WHERE
        uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> Repo_entity.Write.t ->. Caqti_type.unit
  ;;

  let update pool =
    Utils.Database.exec (Database.Label.value pool) update_request
  ;;

  let delete_request =
    let open Caqti_request.Infix in
    {sql|
      DELETE FROM pool_experiments
      WHERE uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> Caqti_type.(string ->. unit)
  ;;

  let delete pool id =
    Utils.Database.exec
      (Pool_database.Label.value pool)
      delete_request
      (id |> Entity.Id.value)
  ;;

  let search_request ids =
    let base =
      {sql|
        SELECT
          LOWER(CONCAT(
            SUBSTR(HEX(pool_experiments.uuid), 1, 8), '-',
            SUBSTR(HEX(pool_experiments.uuid), 9, 4), '-',
            SUBSTR(HEX(pool_experiments.uuid), 13, 4), '-',
            SUBSTR(HEX(pool_experiments.uuid), 17, 4), '-',
            SUBSTR(HEX(pool_experiments.uuid), 21)
          )),
          pool_experiments.title
        FROM pool_experiments
        WHERE pool_experiments.title LIKE $1
      |sql}
    in
    match ids with
    | [] -> base
    | ids ->
      CCList.mapi
        (fun i _ -> Format.asprintf "UNHEX(REPLACE($%i, '-', ''))" (i + 2))
        ids
      |> CCString.concat ","
      |> Format.asprintf
           {sql|
              %s
              AND pool_experiments.uuid NOT IN (%s)
          |sql}
           base
  ;;

  let search pool exclude query =
    let open Caqti_request.Infix in
    let dyn =
      CCList.fold_left
        (fun dyn id ->
          dyn |> Dynparam.add Caqti_type.string (id |> Entity.Id.value))
        Dynparam.(empty |> add Caqti_type.string ("%" ^ query ^ "%"))
        exclude
    in
    let (Dynparam.Pack (pt, pv)) = dyn in
    let request =
      search_request exclude
      |> pt ->* Caqti_type.(Repo_entity.(tup2 RepoId.t Title.t))
    in
    Utils.Database.collect (pool |> Database.Label.value) request pv
  ;;

  let search_multiple_by_id_request ids =
    Format.asprintf
      {sql|
        SELECT
          LOWER(CONCAT(
            SUBSTR(HEX(pool_experiments.uuid), 1, 8), '-',
            SUBSTR(HEX(pool_experiments.uuid), 9, 4), '-',
            SUBSTR(HEX(pool_experiments.uuid), 13, 4), '-',
            SUBSTR(HEX(pool_experiments.uuid), 17, 4), '-',
            SUBSTR(HEX(pool_experiments.uuid), 21)
          )),
          pool_experiments.title
        FROM pool_experiments
        WHERE pool_experiments.uuid in ( %s )
      |sql}
      (CCList.map (fun _ -> Format.asprintf "UNHEX(REPLACE(?, '-', ''))") ids
       |> CCString.concat ",")
  ;;

  let search_multiple_by_id pool ids =
    let open Caqti_request.Infix in
    match ids with
    | [] -> Lwt.return []
    | ids ->
      let dyn =
        CCList.fold_left
          (fun dyn id ->
            dyn |> Dynparam.add Caqti_type.string (id |> Pool_common.Id.value))
          Dynparam.empty
          ids
      in
      let (Dynparam.Pack (pt, pv)) = dyn in
      let request =
        search_multiple_by_id_request ids
        |> pt ->* Caqti_type.(Repo_entity.(tup2 RepoId.t Title.t))
      in
      Utils.Database.collect (pool |> Database.Label.value) request pv
  ;;
end

let find = Sql.find
let find_all = Sql.find_all
let find_of_session = Sql.find_of_session
let find_of_mailing = Sql.find_of_mailing
let session_count = Sql.session_count
let insert = Sql.insert
let update = Sql.update
let delete = Sql.delete
let search = Sql.search
let search_multiple_by_id = Sql.search_multiple_by_id

module Id = Pool_common.Repo.Id
