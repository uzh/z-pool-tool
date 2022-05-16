module RepoEntity = Repo_entity

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
        pool_locations.address,
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
end

let files_to_location pool (RepoEntity.{ id; _ } as location) =
  let open Utils.Lwt_result.Infix in
  Repo_file_mapping.find_by_location pool id ||> to_entity location
;;

let find pool id =
  let open Utils.Lwt_result.Infix in
  (* TODO Implement as transaction *)
  Sql.find pool id >|= files_to_location pool
;;
