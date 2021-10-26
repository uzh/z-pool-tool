open Entity_logo_mapping
module Id = Pool_common.Id
module RepoId = Pool_common.Repo.Id
module File = Pool_common.File
module RepoFile = Pool_common.Repo.File
module Database = Pool_common.Database

let t =
  let encode m =
    Ok
      ( Id.value m.id
      , (stringify_type m.logo_type, (Id.value m.tenant_uuid, m.file)) )
  in
  let decode (id, (logo_type, (tenant_uuid, file))) =
    let open CCResult in
    let* logo_type = logo_type |> type_of_string in
    Ok
      { id = Id.of_string id
      ; logo_type
      ; tenant_uuid = Id.of_string tenant_uuid
      ; file
      }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2 RepoId.t (tup2 string (tup2 RepoId.t RepoFile.t))))
;;

module Write = struct
  open Write

  let t =
    let encode (m : t) =
      Ok
        ( Id.value m.id
        , ( Id.value m.tenant_uuid
          , (Id.value m.asset_uuid, stringify_type m.logo_type) ) )
    in
    let decode (id, (tenant_uuid, (asset_uuid, logo_type))) =
      let open CCResult in
      let* logo_type = logo_type |> type_of_string in
      Ok
        { id = Id.of_string id
        ; tenant_uuid = Id.of_string tenant_uuid
        ; asset_uuid = Id.of_string asset_uuid
        ; logo_type
        }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (tup2 RepoId.t (tup2 RepoId.t (tup2 RepoId.t string))))
  ;;
end

module Sql = struct
  let select_from_tenant_logo_mappings_sql where_fragment =
    let select_from =
      {sql|
          SELECT
            LOWER(CONCAT(
              SUBSTR(HEX(pool_tenant_logo_mappings.uuid), 1, 8), '-',
              SUBSTR(HEX(pool_tenant_logo_mappings.uuid), 9, 4), '-',
              SUBSTR(HEX(pool_tenant_logo_mappings.uuid), 13, 4), '-',
              SUBSTR(HEX(pool_tenant_logo_mappings.uuid), 17, 4), '-',
              SUBSTR(HEX(pool_tenant_logo_mappings.uuid), 21)
            )),
            pool_tenant_logo_mappings.logo_type,
            LOWER(CONCAT(
              SUBSTR(HEX(pool_tenant_logo_mappings.tenant_uuid), 1, 8), '-',
              SUBSTR(HEX(pool_tenant_logo_mappings.tenant_uuid), 9, 4), '-',
              SUBSTR(HEX(pool_tenant_logo_mappings.tenant_uuid), 13, 4), '-',
              SUBSTR(HEX(pool_tenant_logo_mappings.tenant_uuid), 17, 4), '-',
              SUBSTR(HEX(pool_tenant_logo_mappings.tenant_uuid), 21)
            )),
            LOWER(CONCAT(
              SUBSTR(HEX(storage_handles.uuid), 1, 8), '-',
              SUBSTR(HEX(storage_handles.uuid), 9, 4), '-',
              SUBSTR(HEX(storage_handles.uuid), 13, 4), '-',
              SUBSTR(HEX(storage_handles.uuid), 17, 4), '-',
              SUBSTR(HEX(storage_handles.uuid), 21)
            )),
            storage_handles.filename,
            storage_handles.mime,
            storage_handles.created,
            storage_handles.updated
          FROM pool_tenant_logo_mappings
          INNER JOIN storage_handles
            ON pool_tenant_logo_mappings.asset_uuid = storage_handles.uuid
        |sql}
    in
    Format.asprintf "%s %s" select_from where_fragment
  ;;

  let where_fragment =
    {sql|
      WHERE pool_tenant_logo_mappings.tenant_uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
  ;;

  let find_request =
    select_from_tenant_logo_mappings_sql where_fragment
    |> Caqti_request.find Caqti_type.string t
  ;;

  let find pool tenant_id =
    Utils.Database.collect pool find_request (tenant_id |> Pool_common.Id.value)
  ;;

  let find_all_request =
    ""
    |> select_from_tenant_logo_mappings_sql
    |> Caqti_request.find Caqti_type.unit t
  ;;

  let find_all pool = Utils.Database.collect pool find_all_request

  let insert_request =
    {sql|
      INSERT INTO pool_tenant_logo_mappings (
        uuid,
        tenant_uuid,
        asset_uuid,
        logo_type
      ) VALUES (
        UNHEX(REPLACE(?, '-', '')),
        UNHEX(REPLACE(?, '-', '')),
        UNHEX(REPLACE(?, '-', '')),
        ?
      );
    |sql}
    |> Caqti_request.exec Write.t
  ;;

  let insert pool = Utils.Database.exec pool insert_request

  let delete_request =
    {sql|
      DELETE FROM pool_tenant_logo_mappings
      WHERE tenant_uuid = UNHEX(REPLACE(?, '-', ''))
      AND asset_uuid = UNHEX(REPLACE(?, '-', ''));
    |sql}
    |> Caqti_request.exec Caqti_type.(tup2 string string)
  ;;

  let delete pool tenant_uuid asset_uuid =
    Utils.Database.exec
      pool
      delete_request
      (tenant_uuid |> Id.value, asset_uuid |> Id.value)
  ;;
end

let insert_multiple m_list =
  Lwt_list.map_s (Sql.insert (Database.Label.value Database.root)) m_list
;;

let find_by_tenant = Sql.find (Database.Label.value Database.root)
let find_all = Sql.find_all (Database.Label.value Database.root)
let delete = Sql.delete (Database.Label.value Database.root)
