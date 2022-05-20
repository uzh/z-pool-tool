module RepoEntity = Repo_entity
module RepoFileMapping = Repo_file_mapping

let to_entity = RepoEntity.to_entity
let of_entity = RepoEntity.of_entity

module Sql = struct
  let select_sql =
    {sql|
      SELECT
        LOWER(CONCAT(
          SUBSTR(HEX(pool_locations.uuid), 1, 8), '-',
          SUBSTR(HEX(pool_locations.uuid), 9, 4), '-',
          SUBSTR(HEX(pool_locations.uuid), 13, 4), '-',
          SUBSTR(HEX(pool_locations.uuid), 17, 4), '-',
          SUBSTR(HEX(pool_locations.uuid), 21)
        )),
        pool_locations.name,
        pool_locations.description,
        pool_locations.is_virtual,
        pool_locations.room,
        pool_locations.building,
        pool_locations.street,
        pool_locations.zip,
        pool_locations.city,
        LOWER(CONCAT(
          SUBSTR(HEX(storage_handles.uuid), 1, 8), '-',
          SUBSTR(HEX(storage_handles.uuid), 9, 4), '-',
          SUBSTR(HEX(storage_handles.uuid), 13, 4), '-',
          SUBSTR(HEX(storage_handles.uuid), 17, 4), '-',
          SUBSTR(HEX(storage_handles.uuid), 21)
        )),
        pool_locations.link,
        pool_locations.created_at,
        pool_locations.updated_at
      FROM
        pool_locations
      LEFT JOIN storage_handles
        ON pool_locations.asset_id = storage_handles.id
    |sql}
  ;;

  let find_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE
        pool_locations.uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> Format.asprintf "%s\n%s" select_sql
    |> Caqti_type.string ->! RepoEntity.t
  ;;

  let find pool id =
    let open Lwt.Infix in
    Utils.Database.find_opt
      (Pool_database.Label.value pool)
      find_request
      (Pool_common.Id.value id)
    >|= CCOption.to_result Pool_common.Message.(NotFound Field.Location)
  ;;

  let insert_request =
    let open Caqti_request.Infix in
    {sql|
      INSERT INTO pool_locations (
        uuid,
        name,
        description,
        is_virtual,
        room,
        building,
        street,
        zip,
        city,
        link,
        status,
        created_at,
        updated_at
      ) VALUES (
        UNHEX(REPLACE(?, '-', '')),
        ?,
        ?,
        ?,
        ?,
        ?,
        ?
      )
    |sql}
    |> RepoEntity.t ->. Caqti_type.unit
  ;;

  let insert pool =
    Utils.Database.exec (Pool_database.Label.value pool) insert_request
  ;;

  let update_request =
    let open Caqti_request.Infix in
    {sql|
      UPDATE pool_locations
      SET
        name = $2,
        description = $3,
        is_virtual = $4,
        room = $5,
        building = $6,
        street = $7,
        zip = $8,
        city = $9,
        link = $10,
        status = $11,
      WHERE
      pool_locations.uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> RepoEntity.(
         Caqti_type.(
           tup2
             Name.t
             (tup2
                (option Description.t)
                (tup2 Address.t (tup2 (option Link.t) Status.t)))
           ->. unit))
  ;;

  let update pool Entity.{ name; description; address; link; status; _ } =
    Utils.Database.exec
      (Pool_database.Label.value pool)
      update_request
      ( name |> Entity.Name.value
      , ( description |> CCOption.map Entity.Description.value
        , ( address
          , ( link |> CCOption.map Entity.Link.value
            , status |> Entity.Status.show ) ) ) )
  ;;
end

let files_to_location pool (RepoEntity.{ id; _ } as location) =
  let open Utils.Lwt_result.Infix in
  RepoFileMapping.find_by_location pool id ||> to_entity location
;;

let find pool id =
  let open Utils.Lwt_result.Infix in
  (* TODO Implement as transaction *)
  Sql.find pool id >|= files_to_location pool
;;

let insert pool location files =
  let%lwt () = files |> RepoFileMapping.insert_multiple pool in
  location |> of_entity |> Sql.insert pool
;;

let update = Sql.update
