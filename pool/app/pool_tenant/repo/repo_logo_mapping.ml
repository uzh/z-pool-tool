open Entity_logo_mapping
module Id = Pool_common.Id
module RepoId = Pool_common.Repo.Id
module File = Pool_common.File
module RepoFile = Pool_common.Repo.File
module Database = Pool_database

let t =
  let encode m =
    Ok (m.id, (LogoType.to_string m.logo_type, (m.tenant_id, m.file)))
  in
  let decode (id, (logo_type, (tenant_id, file))) =
    let open CCResult in
    map_err (CCFun.const "decode logo mapping read")
    @@ let* logo_type = logo_type |> LogoType.of_string in
       Ok { id; logo_type; tenant_id; file }
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
      Ok (m.id, (m.tenant_id, (m.asset_id, LogoType.to_string m.logo_type)))
    in
    let decode (id, (tenant_id, (asset_id, logo_type))) =
      let open CCResult in
      map_err (CCFun.const "decode logo mapping write")
      @@ let* logo_type = logo_type |> LogoType.of_string in
         Ok { id; tenant_id; asset_id; logo_type }
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
            storage_handles.filesize,
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
    let open Caqti_request.Infix in
    select_from_tenant_logo_mappings_sql where_fragment
    |> Caqti_type.string ->! t
  ;;

  let find pool tenant_id =
    Utils.Database.collect pool find_request (tenant_id |> Pool_common.Id.value)
  ;;

  let find_all_request =
    let open Caqti_request.Infix in
    "" |> select_from_tenant_logo_mappings_sql |> Caqti_type.unit ->! t
  ;;

  let find_all pool = Utils.Database.collect pool find_all_request

  let insert_request =
    let open Caqti_request.Infix in
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
      )
    |sql}
    |> Write.t ->. Caqti_type.unit
  ;;

  let insert pool = Utils.Database.exec pool insert_request

  let delete_request =
    let open Caqti_request.Infix in
    {sql|
      DELETE FROM pool_tenant_logo_mappings
      WHERE tenant_uuid = UNHEX(REPLACE(?, '-', ''))
      AND asset_uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> Caqti_type.(tup2 string string ->. unit)
  ;;

  let delete pool tenant_id asset_id =
    Utils.Database.exec
      pool
      delete_request
      (tenant_id |> Id.value, asset_id |> Id.value)
  ;;
end

let insert_multiple m_list =
  Lwt_list.map_s (Sql.insert Database.(Label.value root)) m_list
;;

let find_by_tenant = Sql.find Database.(Label.value root)
let find_all = Sql.find_all Database.(Label.value root)
let delete = Sql.delete Database.(Label.value root)
