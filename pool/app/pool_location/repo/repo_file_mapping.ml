open Entity_file_mapping

module Id = struct
  include Id

  let t = Pool_common.Repo.Id.t
end

module Label = struct
  include Label

  let t = Caqti_type.string
end

module Description = struct
  include Description

  let t = Caqti_type.string
end

module File = struct
  include Pool_common.File

  let t = Pool_common.Repo.File.t
end

let file =
  let encode m =
    Ok (m.id, (Label.show m.label, (m.language, (m.description, m.file))))
  in
  let decode (id, (label, (language, (description, file)))) =
    let open CCResult in
    map_err
      (CCFun.const
         Pool_common.(
           Utils.error_to_string Language.En Message.(Decode Field.FileMapping)))
    @@ Ok { id; label = label |> Label.read; language; description; file }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2
         Id.t
         (tup2
            Label.t
            (tup2 Pool_common.Repo.Language.t (tup2 Description.t File.t)))))
;;

module Write = struct
  type file =
    { id : Id.t
    ; label : Label.t
    ; language : Pool_common.Language.t
    ; description : Description.t
    ; asset_id : Pool_common.Id.t
    ; location_id : Pool_common.Id.t
    }
  [@@deriving eq, show]

  let file =
    let encode (m : file) =
      Ok
        ( m.id
        , ( Label.show m.label
          , (m.language, (m.description, (m.asset_id, m.location_id))) ) )
    in
    let decode (id, (label, (language, (description, (asset_id, location_id)))))
      =
      Ok
        { id
        ; label = Label.read label
        ; language
        ; description
        ; asset_id
        ; location_id
        }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (tup2
           Id.t
           (tup2
              Label.t
              (tup2
                 Pool_common.Repo.Language.t
                 (tup2 Description.t (tup2 Id.t Id.t))))))
  ;;
end

let of_entity (location : Entity.t) (m : file) : Write.file =
  Write.
    { id = m.id
    ; label = m.label
    ; language = m.language
    ; description = m.description
    ; asset_id = m.file.File.id
    ; location_id = location.Entity.id
    }
;;

module Sql = struct
  let select_from_tenant_logo_mappings_sql where_fragment =
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
            pool_location_file_mappings.description,
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
    |> select_from_tenant_logo_mappings_sql
    |> Caqti_type.string ->! file
  ;;

  let find pool mapping_id =
    let open Lwt.Infix in
    Utils.Database.find_opt
      (Pool_database.Label.value pool)
      find_request
      (Pool_common.Id.value mapping_id)
    >|= CCOption.to_result Pool_common.Message.(NotFound Field.FileMapping)
  ;;

  let find_by_location_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE pool_location_file_mappings.location_id = (SELECT id FROM pool_locations WHERE uuid = UNHEX(REPLACE(?, '-', '')))
    |sql}
    |> select_from_tenant_logo_mappings_sql
    |> Caqti_type.string ->* file
  ;;

  let find_by_location pool location =
    Utils.Database.collect
      (Pool_database.Label.value pool)
      find_by_location_request
      (Pool_common.Id.value location)
  ;;

  let insert_request =
    let open Caqti_request.Infix in
    {sql|
      INSERT INTO pool_location_file_mappings (
        uuid,
        label,
        language,
        description,
        asset_id,
        location_id
      ) VALUES (
        UNHEX(REPLACE($1, '-', '')),
        $2,
        $3,
        $4,
        (SELECT id FROM storage_handles WHERE uuid = UNHEX(REPLACE($5, '-', ''))),
        (SELECT id FROM pool_locations WHERE uuid = UNHEX(REPLACE($6, '-', '')))
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
    |> Caqti_type.(string ->. unit)
  ;;

  let delete pool Entity_file_mapping.{ id; file; _ } =
    (* TODO: Transaction *)
    let%lwt () =
      Utils.Database.exec
        (Pool_database.Label.value pool)
        delete_request
        (Id.value id)
    in
    Service.Storage.delete
      ~ctx:(Pool_tenant.to_ctx pool)
      (Id.value file.File.id)
  ;;
end

let insert_multiple pool ms = Lwt_list.iter_s (Sql.insert pool) ms
let find_by_tenant = Sql.find
let find_by_location = Sql.find_by_location
let delete = Sql.delete
