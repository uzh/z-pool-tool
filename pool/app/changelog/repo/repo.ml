open Caqti_request.Infix
module Dynparam = Database.Dynparam

let sql_select_columns =
  [ Entity.Id.sql_select_fragment ~field:"pool_change_log.uuid"
  ; "pool_change_log.model"
  ; Entity.Id.sql_select_fragment ~field:"pool_change_log.user_uuid"
  ; "pool_change_log.changes"
  ; "pool_change_log.created_at"
  ; "pool_change_log.updated_at"
  ]
;;

let find_request_sql ?(count = false) =
  let columns =
    if count then "COUNT(*)" else sql_select_columns |> CCString.concat ", "
  in
  Format.asprintf {sql|SELECT %s FROM pool_change_log %s |sql} columns
;;

let find_by_model ?query pool field =
  let where =
    ( "pool_change_log.model = ?"
    , Dynparam.(empty |> add Repo_entity.Field.t field) )
  in
  Query.collect_and_count
    pool
    query
    ~select:find_request_sql
    ~where
    Repo_entity.t
;;

let insert_request =
  {sql|
    INSERT INTO pool_change_log (
      uuid,
      model,
      user_uuid,
      changes,
      created_at
    ) VALUES (
      UNHEX(REPLACE($1, '-', '')),
      $2,
      UNHEX(REPLACE($3, '-', '')),
      $4,
      $5
    )
  |sql}
  |> Repo_entity.t ->. Caqti_type.unit
;;

let insert pool = Database.exec pool insert_request
