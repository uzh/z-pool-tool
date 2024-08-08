let sql_select_columns =
  [ Entity.Id.sql_select_fragment ~field:"pool_change_log.uuid"
  ; "pool_change_log.model"
  ; Entity.Id.sql_select_fragment ~field:"pool_change_log.user_uuid"
  ; "pool_change_log.changes"
  ; "pool_change_log.created_at"
  ; "pool_change_log.updated_at"
  ]
;;

let insert_request =
  let open Caqti_request.Infix in
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
