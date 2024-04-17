open CCFun.Infix
open Entity_logo_mapping
module Id = Pool_common.Id
module RepoId = Pool_common.Repo.Id
module File = Pool_common.File
module RepoFile = Pool_common.Repo.File

module LogoType = struct
  include LogoType

  let t =
    Caqti_type.(
      custom
        ~encode:(to_string %> CCResult.return)
        ~decode:(of_string %> CCResult.map_err Pool_message.Error.show)
        string)
  ;;
end

let t =
  let open Database.Caqti_encoders in
  let encode m : ('a Data.t, string) result =
    Ok Data.[ m.id; m.logo_type; m.tenant_id; m.file ]
  in
  let decode (id, (logo_type, (tenant_id, (file, ())))) =
    Ok { id; logo_type; tenant_id; file }
  in
  custom ~encode ~decode Schema.[ RepoId.t; LogoType.t; RepoId.t; RepoFile.t ]
;;

module Write = struct
  open Write

  let t =
    let open Database.Caqti_encoders in
    let encode m : ('a Data.t, string) result =
      Ok Data.[ m.Write.id; m.tenant_id; m.asset_id; m.logo_type ]
    in
    let decode (id, (tenant_id, (asset_id, (logo_type, ())))) =
      Ok { id; tenant_id; asset_id; logo_type }
    in
    custom ~encode ~decode Schema.[ RepoId.t; RepoId.t; RepoId.t; LogoType.t ]
  ;;
end

module Sql = struct
  let sql_select_columns =
    [ Id.sql_select_fragment ~field:"pool_tenant_logo_mappings.uuid"
    ; "pool_tenant_logo_mappings.logo_type"
    ; Id.sql_select_fragment ~field:"pool_tenant_logo_mappings.tenant_uuid"
    ; Id.sql_select_fragment ~field:"storage_handles.uuid"
    ; "storage_handles.filename"
    ; "storage_handles.filesize"
    ; "storage_handles.mime"
    ; "storage_handles.created"
    ; "storage_handles.updated"
    ]
  ;;

  let joins =
    {sql|
      INNER JOIN storage_handles
        ON pool_tenant_logo_mappings.asset_uuid = storage_handles.uuid
    |sql}
  ;;

  let select_from_tenant_logo_mappings_sql where_fragment =
    let columns = sql_select_columns |> CCString.concat ",\n" in
    [%string
      {sql|
        SELECT
          %{columns}
        FROM pool_tenant_logo_mappings
        %{joins}
        %{where_fragment}
      |sql}]
  ;;

  let where_fragment =
    [%string
      {sql|
        WHERE pool_tenant_logo_mappings.tenant_uuid = %{Id.sql_value_fragment "?"}
      |sql}]
  ;;

  let find_request =
    let open Caqti_request.Infix in
    select_from_tenant_logo_mappings_sql where_fragment
    |> Caqti_type.string ->! t
  ;;

  let find pool tenant_id =
    Database.collect pool find_request (tenant_id |> Pool_common.Id.value)
  ;;

  let find_all_request =
    let open Caqti_request.Infix in
    "" |> select_from_tenant_logo_mappings_sql |> Caqti_type.unit ->! t
  ;;

  let find_all pool = Database.collect pool find_all_request

  let insert_request =
    let open Caqti_request.Infix in
    [%string
      {sql|
        INSERT INTO pool_tenant_logo_mappings (
          uuid,
          tenant_uuid,
          asset_uuid,
          logo_type
        ) VALUES (
          %{Id.sql_value_fragment "?"},
          %{Id.sql_value_fragment "?"},
          %{Id.sql_value_fragment "?"},
          ?
        )
      |sql}]
    |> Write.t ->. Caqti_type.unit
  ;;

  let insert pool = Database.exec pool insert_request

  let delete_request =
    let open Caqti_request.Infix in
    [%string
      {sql|
        DELETE FROM pool_tenant_logo_mappings
        WHERE tenant_uuid = %{Id.sql_value_fragment "?"}
        AND asset_uuid = %{Id.sql_value_fragment "?"}
      |sql}]
    |> Caqti_type.(t2 string string ->. unit)
  ;;

  let delete pool tenant_id asset_id =
    Database.exec
      pool
      delete_request
      (tenant_id |> Id.value, asset_id |> Id.value)
  ;;
end

let insert_multiple m_list = Lwt_list.map_s (Sql.insert Database.(root)) m_list
let find_by_tenant = Sql.find Database.(root)
let find_all = Sql.find_all Database.(root)
let delete = Sql.delete Database.(root)
