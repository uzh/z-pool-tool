open Caqti_request.Infix
module Dynparam = Database.Dynparam

let sql_select_columns =
  [ Entity.Id.sql_select_fragment ~field:"pool_change_log.uuid"
  ; "pool_change_log.model"
  ; Entity.Id.sql_select_fragment ~field:"pool_change_log.entity_uuid"
  ; Entity.Id.sql_select_fragment ~field:"pool_change_log.user_uuid"
  ; "user_users.email"
  ; "pool_change_log.changes"
  ; "pool_change_log.created_at"
  ]
;;

let joins =
  {sql| INNER JOIN user_users ON pool_change_log.user_uuid = user_users.uuid |sql}
;;

let find_request_sql ?(count = false) =
  let columns =
    if count then "COUNT(*)" else sql_select_columns |> CCString.concat ", "
  in
  Format.asprintf {sql|SELECT %s FROM pool_change_log %s %s |sql} columns joins
;;

let find_by_model field ?query pool entity_uuid =
  let open Repo_entity in
  let where =
    ( {sql| 
        pool_change_log.model = $1 
          AND 
        pool_change_log.entity_uuid = UNHEX(REPLACE($2, '-', ''))
      |sql}
    , Dynparam.(empty |> add Field.t field |> add RepoId.t entity_uuid) )
  in
  Query.collect_and_count pool query ~select:find_request_sql ~where t
;;

let insert_request =
  {sql|
    INSERT INTO pool_change_log (
      uuid,
      model,
      entity_uuid,
      user_uuid,
      changes,
      created_at
    ) VALUES (
      UNHEX(REPLACE($1, '-', '')),
      $2,
      UNHEX(REPLACE($3, '-', '')),
      UNHEX(REPLACE($4, '-', '')),
      $5,
      $6
    )
  |sql}
  |> Repo_entity.Write.t ->. Caqti_type.unit
;;

let insert pool = Database.exec pool insert_request
