module Dynparam = Database.Dynparam

let sql_select_columns =
  [ Entity.Id.sql_select_fragment ~field:"pool_announcements.uuid"
  ; "pool_announcements.text"
  ; "pool_announcements.start_at"
  ; "pool_announcements.end_at"
  ; "pool_announcements.created_at"
  ; "pool_announcements.updated_at"
  ]
;;

let insert_request =
  let open Caqti_request.Infix in
  {sql|
    INSERT INTO pool_announcements (
      uuid,
      text,
      start_at,
      end_at
    ) VALUES (
      UNHEX(REPLACE($1, '-', '')),
      $2,
      $3,
      $4
    )
  |sql}
  |> Repo_entity.Write.t ->. Caqti_type.unit
;;

let insert pool = Database.exec pool insert_request

let update_request =
  let open Caqti_request.Infix in
  {sql|
    UPDATE pool_announcements
    SET
      text = $2,
      start_at = $3,
      end_at = $4
    WHERE
      uuid = UNHEX(REPLACE($1, '-', ''))
  |sql}
  |> Repo_entity.Write.t ->. Caqti_type.unit
;;

let update pool = Database.exec pool update_request

let find_request_sql ?(count = false) where_fragment =
  let columns =
    if count then "COUNT(*)" else CCString.concat ", " sql_select_columns
  in
  Format.asprintf
    {sql|SELECT %s FROM pool_announcements %s|sql}
    columns
    where_fragment
;;

let find_request =
  let open Caqti_request.Infix in
  {sql|
    WHERE pool_announcements.uuid = UNHEX(REPLACE(?, '-', ''))
  |sql}
  |> find_request_sql
  |> Pool_common.Repo.Id.t ->! Repo_entity.t
;;

let find pool id =
  let open Utils.Lwt_result.Infix in
  Database.find_opt pool find_request id
  ||> CCOption.to_result Pool_message.(Error.NotFound Field.Announcement)
;;

let all ?query pool =
  Query.collect_and_count pool query ~select:find_request_sql Repo_entity.t
;;
