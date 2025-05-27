let stored_file =
  let encode m =
    let open Sihl.Contract.Storage in
    let { file; blob } = m in
    let { id; filename; filesize; mime } = file in
    Ok (id, (filename, (filesize, (mime, blob))))
  in
  let decode (id, (filename, (filesize, (mime, blob)))) =
    let open Sihl.Contract.Storage in
    let file = { id; filename; filesize; mime } in
    Ok { file; blob }
  in
  Caqti_type.(
    custom ~encode ~decode Caqti_type.(t2 string (t2 string (t2 int (t2 string string)))))
;;

let insert_request =
  let open Caqti_request.Infix in
  {sql|
      INSERT INTO storage_handles (
        uuid,
        filename,
        filesize,
        mime,
        asset_blob
      ) VALUES (
        UNHEX(REPLACE(?, '-', '')),
        ?,
        ?,
        ?,
        UNHEX(REPLACE(?, '-', ''))
      )
    |sql}
  |> stored_file ->. Caqti_type.unit
;;

let insert_file label file = Database.exec label insert_request file

let update_file_request =
  let open Caqti_request.Infix in
  {sql|
      UPDATE storage_handles SET
        filename = $2,
        filesize = $3,
        mime = $4,
        asset_blob = UNHEX(REPLACE($5, '-', ''))
      WHERE
        storage_handles.uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
  |> stored_file ->. Caqti_type.unit
;;

let update_file label file = Database.exec label update_file_request file

let get_file_request =
  let open Caqti_request.Infix in
  {sql|
      SELECT
        LOWER(CONCAT(
        SUBSTR(HEX(uuid), 1, 8), '-',
        SUBSTR(HEX(uuid), 9, 4), '-',
        SUBSTR(HEX(uuid), 13, 4), '-',
        SUBSTR(HEX(uuid), 17, 4), '-',
        SUBSTR(HEX(uuid), 21)
        )),
      filename,
      filesize,
      mime,
      LOWER(CONCAT(
        SUBSTR(HEX(asset_blob), 1, 8), '-',
        SUBSTR(HEX(asset_blob), 9, 4), '-',
        SUBSTR(HEX(asset_blob), 13, 4), '-',
        SUBSTR(HEX(asset_blob), 17, 4), '-',
        SUBSTR(HEX(asset_blob), 21)
        ))
      FROM storage_handles
      WHERE storage_handles.uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
  |> Caqti_type.string ->? stored_file
;;

let get_file label id = Database.find_opt label get_file_request id

let delete_file_request =
  let open Caqti_request.Infix in
  {sql|
      DELETE FROM storage_handles
      WHERE storage_handles.uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
  |> Caqti_type.(string ->. unit)
;;

let delete_file label id = Database.exec label delete_file_request id

let get_blob_request =
  let open Caqti_request.Infix in
  {sql|
      SELECT
        asset_data
      FROM storage_blobs
      WHERE storage_blobs.uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
  |> Caqti_type.(string ->? string)
;;

let get_blob label id = Database.find_opt label get_blob_request id

let insert_blob_request =
  let open Caqti_request.Infix in
  {sql|
      INSERT INTO storage_blobs (
        uuid,
        asset_data
      ) VALUES (
        UNHEX(REPLACE(?, '-', '')),
        ?
      )
    |sql}
  |> Caqti_type.(t2 string string ->. unit)
;;

let insert_blob label ~id blob = Database.exec label insert_blob_request (id, blob)

let update_blob_request =
  let open Caqti_request.Infix in
  {sql|
      UPDATE storage_blobs SET
        asset_data = $2
      WHERE
        storage_blobs.uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
  |> Caqti_type.(t2 string string ->. unit)
;;

let update_blob label ~id blob = Database.exec label update_blob_request (id, blob)

let delete_blob_request =
  let open Caqti_request.Infix in
  {sql|
      DELETE FROM storage_blobs
      WHERE
        storage_blobs.uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
  |> Caqti_type.(string ->. unit)
;;

let delete_blob label id = Database.exec label delete_blob_request id

let clean_handles_request =
  let open Caqti_request.Infix in
  "TRUNCATE storage_handles" |> Caqti_type.(unit ->. unit)
;;

let clean_handles label () = Database.exec label clean_handles_request ()

let clean_blobs_request =
  let open Caqti_request.Infix in
  "TRUNCATE storage_blobs" |> Caqti_type.(unit ->. unit)
;;

let clean_blobs label () = Database.exec label clean_blobs_request ()

let fix_collation =
  Database.Migration.Step.create
    ~label:"fix collation"
    {sql|
        SET collation_server = 'utf8mb4_unicode_ci'
      |sql}
;;

let create_blobs_table =
  Database.Migration.Step.create
    ~label:"create blobs table"
    {sql|
        CREATE TABLE IF NOT EXISTS storage_blobs (
          id BIGINT UNSIGNED AUTO_INCREMENT,
          uuid BINARY(16) NOT NULL,
          asset_data MEDIUMBLOB NOT NULL,
          created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
          updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
        PRIMARY KEY (id),
        CONSTRAINT unique_uuid UNIQUE KEY (uuid)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
      |sql}
;;

let create_handles_table =
  Database.Migration.Step.create
    ~label:"create handles table"
    {sql|
        CREATE TABLE IF NOT EXISTS storage_handles (
          id BIGINT UNSIGNED AUTO_INCREMENT,
          uuid BINARY(16) NOT NULL,
          filename VARCHAR(255) NOT NULL,
          filesize BIGINT UNSIGNED,
          mime VARCHAR(128) NOT NULL,
          asset_blob BINARY(16) NOT NULL,
          created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
          updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
        PRIMARY KEY (id),
        CONSTRAINT unique_uuid UNIQUE KEY (uuid)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
      |sql}
;;

let migration () =
  Database.Migration.(
    empty "storage"
    |> add_step fix_collation
    |> add_step create_blobs_table
    |> add_step create_handles_table)
;;

let register_migration () = Database.Migration.register_migration (migration ())

let register_cleaner () =
  let cleaner label () =
    let%lwt () = clean_handles label () in
    clean_blobs label ()
  in
  Sihl.Cleaner.register_cleaner (fun ?ctx () ->
    cleaner
      CCOption.(
        map Database.of_ctx_exn ctx
        |> get_exn_or Pool_message.(Error.(NotFound Field.Context |> show)))
      ())
;;
