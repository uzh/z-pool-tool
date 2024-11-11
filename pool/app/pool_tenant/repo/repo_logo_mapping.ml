open CCFun.Infix
open Caqti_request.Infix
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
    select_from_tenant_logo_mappings_sql where_fragment
    |> Pool_common.Repo.Id.t ->! t
  ;;

  let find pool = Database.collect pool find_request

  let find_all_request =
    "" |> select_from_tenant_logo_mappings_sql |> Caqti_type.unit ->! t
  ;;

  let find_all pool = Database.collect pool find_all_request

  let insert_request =
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
    [%string
      {sql|
        DELETE FROM pool_tenant_logo_mappings
        WHERE tenant_uuid = %{Id.sql_value_fragment "?"}
        AND asset_uuid = %{Id.sql_value_fragment "?"}
      |sql}]
    |> Caqti_type.(t2 RepoId.t RepoId.t ->. unit)
  ;;

  let delete pool = CCFun.curry (Database.exec pool delete_request)
end

let insert_multiple = Lwt_list.iter_s (Sql.insert Database.Pool.Root.label)
let find_by_tenant = Sql.find Database.Pool.Root.label
let find_all = Sql.find_all Database.Pool.Root.label
let delete = Sql.delete Database.Pool.Root.label
