open Entity_file_mapping

module Id = struct
  include Id

  let t = Pool_common.Repo.Id.t
end

module Label = Pool_common.Repo.Model.SelectorType (Label)

module File = struct
  include Pool_common.File

  let t = Pool_common.Repo.File.t
end

let file =
  let encode m = Ok (m.id, (m.label, (m.language, m.file))) in
  let decode (id, (label, (language, file))) =
    Ok { id; label; language; file }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2 Id.t (tup2 Label.t (tup2 Pool_common.Repo.Language.t File.t))))
;;

module Write = struct
  include Write

  let file =
    let encode (m : file) =
      Ok (m.id, (m.label, (m.language, (m.asset_id, m.location_id))))
    in
    let decode (id, (label, (language, (asset_id, location_id)))) =
      Ok { id; label; language; asset_id; location_id }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (tup2
           Id.t
           (tup2 Label.t (tup2 Pool_common.Repo.Language.t (tup2 Id.t Id.t)))))
  ;;
end

let of_entity (location : Entity.t) (m : file) : Write.file =
  Write.
    { id = m.id
    ; label = m.label
    ; language = m.language
    ; asset_id = m.file.File.id
    ; location_id = location.Entity.id
    }
;;

module Sql = struct
  let select_from_location_file_mappings_sql where_fragment =
    let select_from =
      {sql|
          SELECT
            LOWER(CONCAT(
              SUBSTR(HEX(pool_location_file_mappings.uuid), 1, 8), '-',
              SUBSTR(HEX(pool_location_file_mappings.uuid), 9, 4), '-',
              SUBSTR(HEX(pool_location_file_mappings.uuid), 13, 4), '-',
              SUBSTR(HEX(pool_location_file_mappings.uuid), 17, 4), '-',
              SUBSTR(HEX(pool_location_file_mappings.uuid), 21)
            )),
            pool_location_file_mappings.label,
            pool_location_file_mappings.language,
            LOWER(CONCAT(
              SUBSTR(HEX(storage_handles.uuid), 1, 8), '-',
              SUBSTR(HEX(storage_handles.uuid), 9, 4), '-',
              SUBSTR(HEX(storage_handles.uuid), 13, 4), '-',
              SUBSTR(HEX(storage_handles.uuid), 17, 4), '-',
              SUBSTR(HEX(storage_handles.uuid), 21)
            )),
            storage_handles.filename,
            storage_handles.filesize,
            storage_handles.mime,
            storage_handles.created,
            storage_handles.updated
          FROM pool_location_file_mappings
          INNER JOIN storage_handles
            ON pool_location_file_mappings.asset_id = storage_handles.id
        |sql}
    in
    Format.asprintf "%s %s" select_from where_fragment
  ;;

  let find_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE pool_location_file_mappings.uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> select_from_location_file_mappings_sql
    |> Id.t ->! file
  ;;

  let find pool mapping_id =
    let open Utils.Lwt_result.Infix in
    Utils.Database.find_opt
      (Pool_database.Label.value pool)
      find_request
      mapping_id
    ||> CCOption.to_result Pool_common.Message.(NotFound Field.FileMapping)
  ;;

  let find_by_location_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE pool_location_file_mappings.location_id = (SELECT id FROM pool_locations WHERE uuid = UNHEX(REPLACE(?, '-', '')))
    |sql}
    |> select_from_location_file_mappings_sql
    |> Repo_entity.Id.t ->* file
  ;;

  let find_by_location pool location =
    Utils.Database.collect
      (Pool_database.Label.value pool)
      find_by_location_request
      location
  ;;

  let insert_request =
    let open Caqti_request.Infix in
    {sql|
      INSERT INTO pool_location_file_mappings (
        uuid,
        label,
        language,
        asset_id,
        location_id
      ) VALUES (
        UNHEX(REPLACE($1, '-', '')),
        $2,
        $3,
        (SELECT id FROM storage_handles WHERE uuid = UNHEX(REPLACE($4, '-', ''))),
        (SELECT id FROM pool_locations WHERE uuid = UNHEX(REPLACE($5, '-', '')))
      )
    |sql}
    |> Write.file ->. Caqti_type.unit
  ;;

  let insert pool =
    Utils.Database.exec (Pool_database.Label.value pool) insert_request
  ;;

  let delete_request =
    let open Caqti_request.Infix in
    {sql|
      DELETE FROM pool_location_file_mappings
      WHERE uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> Id.t ->. Caqti_type.unit
  ;;

  let delete pool id =
    let open Utils.Lwt_result.Infix in
    (* TODO: Transaction *)
    let%lwt { file; _ } = find pool id ||> Pool_common.Utils.get_or_failwith in
    let%lwt () =
      Utils.Database.exec (Pool_database.Label.value pool) delete_request id
    in
    Service.Storage.delete
      ~ctx:(Pool_tenant.to_ctx pool)
      (Id.value file.File.id)
  ;;
end

let insert = Sql.insert

let find_storage_blob database_label id =
  let open Utils.Lwt_result.Infix in
  let ctx = Pool_tenant.to_ctx database_label in
  Sql.find database_label id
  |>> fun { file; _ } ->
  file.Pool_common.File.id |> Pool_common.Id.value |> Service.Storage.find ~ctx
;;

let find_by_location = Sql.find_by_location
let delete = Sql.delete
