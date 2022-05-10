open Entity
module Id = Pool_common.Id
module RepoId = Pool_common.Repo.Id
module Database = Pool_database

(* TODO [aerben] these circumvent our smart constructors, good? *)
let t =
  let encode m =
    Ok
      ( Id.value m.id
      , ( m.start
        , ( m.duration
          , ( m.description
            , ( m.max_participants
              , ( m.min_participants
                , (m.overbook, (m.canceled_at, (m.created_at, m.updated_at))) )
              ) ) ) ) )
  in
  let decode
      ( id
      , ( start
        , ( duration
          , ( description
            , ( max_participants
              , ( min_participants
                , (overbook, (canceled_at, (created_at, updated_at))) ) ) ) ) )
      )
    =
    Ok
      { id = Id.of_string id
      ; start
      ; duration
      ; description
      ; max_participants
      ; min_participants
      ; overbook
      ; canceled_at
      ; created_at
      ; updated_at
      }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2
         RepoId.t
         (tup2
            ptime
            (tup2
               ptime_span
               (tup2
                  (option string)
                  (tup2
                     int
                     (tup2
                        int
                        (tup2 int (tup2 (option ptime) (tup2 ptime ptime))))))))))
;;

module Write = struct
  let t =
    let encode m =
      Ok
        ( Id.value m.id
        , ( m.start
          , ( m.duration
            , ( m.description
              , ( m.max_participants
                , (m.min_participants, (m.overbook, m.canceled_at)) ) ) ) ) )
    in
    let decode _ = failwith "Write only model" in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (tup2
           RepoId.t
           (tup2
              ptime
              (tup2
                 ptime_span
                 (tup2
                    (option string)
                    (tup2 int (tup2 int (tup2 int (option ptime)))))))))
  ;;
end

module Sql = struct
  let find_sql where =
    let select =
      {sql|
        SELECT
          LOWER(CONCAT(
            SUBSTR(HEX(uuid), 1, 8), '-',
            SUBSTR(HEX(uuid), 9, 4), '-',
            SUBSTR(HEX(uuid), 13, 4), '-',
            SUBSTR(HEX(uuid), 17, 4), '-',
            SUBSTR(HEX(uuid), 21)
          )),
          start,
          duration,
          description,
          max_participants,
          min_participants,
          overbook,
          canceled_at,
          created_at,
          updated_at
        FROM pool_sessions
      |sql}
    in
    Format.asprintf "%s %s" select where
  ;;

  let find_request =
    let open Caqti_request.Infix in
    {sql|
        WHERE uuid = UNHEX(REPLACE(?, '-', ''))
      |sql}
    |> find_sql
    |> Caqti_type.string ->! t
  ;;

  let find pool id =
    let open Lwt.Infix in
    Utils.Database.find_opt
      (Database.Label.value pool)
      find_request
      (Pool_common.Id.value id)
    >|= CCOption.to_result Pool_common.Message.(NotFound Field.Session)
  ;;

  let find_all_for_experiment_request =
    let open Caqti_request.Infix in
    (* TODO [aerben] order by what here? *)
    {sql|
        WHERE experiment_uuid = UNHEX(REPLACE(?, '-', ''))
        ORDER BY start
      |sql}
    |> find_sql
    |> Caqti_type.string ->* t
  ;;

  let find_all_for_experiment pool id =
    Utils.Database.collect
      (Database.Label.value pool)
      find_all_for_experiment_request
      (Pool_common.Id.value id)
  ;;

  let insert_request =
    let open Caqti_request.Infix in
    {sql|
      INSERT INTO pool_sessions (
        uuid,
        experiment_uuid,
        start,
        duration,
        description,
        max_participants,
        min_participants,
        overbook,
        canceled_at,
        created_at,
        updated_at
      ) VALUES (
        UNHEX(REPLACE($2, '-', '')),
        UNHEX(REPLACE($1, '-', '')),
        $3,
        $4,
        $5,
        $6,
        $7,
        $8,
        $9,
        $10,
        $11
      )
    |sql}
    |> Caqti_type.(tup2 string t ->. unit)
  ;;

  let insert pool =
    Utils.Database.exec (Database.Label.value pool) insert_request
  ;;

  let update_request =
    let open Caqti_request.Infix in
    {sql|
      UPDATE pool_sessions
      SET
        start = $2,
        duration = $3,
        description = $4,
        max_participants = $5,
        min_participants = $6,
        overbook = $7,
        canceled_at = $8
      WHERE
        uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> Write.t ->. Caqti_type.unit
  ;;

  let update pool =
    Utils.Database.exec (Database.Label.value pool) update_request
  ;;

  let delete_request =
    let open Caqti_request.Infix in
    {sql|
      DELETE FROM pool_sessions
      WHERE uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> Caqti_type.(string ->. unit)
  ;;

  let delete pool id =
    Utils.Database.exec
      (Pool_database.Label.value pool)
      delete_request
      (Pool_common.Id.value id)
  ;;
end

let find = Sql.find
let find_all_for_experiment = Sql.find_all_for_experiment
let insert = Sql.insert
let update = Sql.update
let delete = Sql.delete
