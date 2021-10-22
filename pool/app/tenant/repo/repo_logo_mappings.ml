module Id = Pool_common.Id
module RepoId = Pool_common.Repo.Id
module File = Pool_common.File
module RepoFile = Pool_common.Repo.File
module Database = Pool_common.Database

let stringify_type = function
  | `Partner -> "partner"
  | `Tenant -> "tenant"
;;

let type_of_string = function
  | "partner" -> Ok `Partner
  | "tenant" -> Ok `Tenant
  | _ -> Error "Unknown logo type"
;;

module Entity = struct
  type t =
    { logo_type : [ `Partner | `Tenant ]
    ; file : RepoFile.t
    }

  let t =
    let encode m = Ok (stringify_type m.logo_type, m.file) in
    let decode (logo_type, file) =
      let open CCResult in
      let* logo_type = logo_type |> type_of_string in
      Ok { logo_type; file }
    in
    Caqti_type.(custom ~encode ~decode (tup2 string RepoFile.t))
  ;;

  module Write = struct
    type t =
      { tenant_uuid : Id.t
      ; asset_uuid : Id.t
      ; logo_type : string
      }

    let t =
      let encode m =
        Ok (Id.value m.tenant_uuid, (Id.value m.asset_uuid, m.logo_type))
      in
      let decode (tenant_uuid, (asset_uuid, logo_type)) =
        Ok
          { tenant_uuid = Id.of_string tenant_uuid
          ; asset_uuid = Id.of_string asset_uuid
          ; logo_type
          }
      in
      Caqti_type.(custom ~encode ~decode (tup2 RepoId.t (tup2 RepoId.t string)))
    ;;
  end
end

module Sql = struct
  let select_from_tenant_logo_mappings_sql where_fragment =
    let select_from =
      {sql|
          SELECT
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
            storage_handles.updated,
          FROM pool_tenant_logo_mappings
          LEFT JOIN storage_handles
            ON pool_tenant_logo_mappings.asset_uuid = storage_handles.uuid
        |sql}
    in
    Format.asprintf "%s %s" select_from where_fragment
  ;;

  let find_fragment =
    {sql|
      WHERE pool_tenant_logo_mappings.asset_uuid = UNHEX(REPLACE(?, '-', ''))
      WHERE pool_tenant_logo_mappings.logo_type = ?
    |sql}
  ;;

  let find_request =
    select_from_tenant_logo_mappings_sql find_fragment
    |> Caqti_request.find Caqti_type.(tup2 string string) RepoFile.t
  ;;

  let find pool tenant_id logo_type =
    Utils.Database.collect
      pool
      find_request
      (tenant_id |> Pool_common.Id.value, logo_type |> stringify_type)
  ;;

  let insert_request =
    {sql|
      INSERT INTO pool_tenant_logo_mappings (
        uuid,
        tenant_uuid,
        asset_uuid,
        logo_type,
      ) VALUES (
        UNHEX(REPLACE(?, '-', '')),
        UNHEX(REPLACE(?, '-', '')),
        UNHEX(REPLACE(?, '-', '')),
        ?
      );
    |sql}
    |> Caqti_request.exec Entity.t
  ;;

  let insert = failwith "Todo"
  (* let insert pool logo_type m = Utils.Database.exec pool insert_request
     (Entity.extract m logo_type) ;; *)
end

(* let insert_partner_logo = Sql.insert (Database.Label.value Database.root)
   `Partner ;;

   let insert_tenant_logo = Sql.insert (Database.Label.value Database.root)
   `Tenant *)
let find_partner_logos tenant_id =
  Sql.find (Database.Label.value Database.root) tenant_id `Partner
;;

let find_tenant_logos tenant_id =
  Sql.find (Database.Label.value Database.root) tenant_id `Tenant
;;
