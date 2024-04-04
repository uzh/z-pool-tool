module Database = Database

let select_sql where =
  Format.asprintf
    {sql|
    SELECT
      LOWER(CONCAT(
        SUBSTR(HEX(pool_custom_field_groups.uuid), 1, 8), '-',
        SUBSTR(HEX(pool_custom_field_groups.uuid), 9, 4), '-',
        SUBSTR(HEX(pool_custom_field_groups.uuid), 13, 4), '-',
        SUBSTR(HEX(pool_custom_field_groups.uuid), 17, 4), '-',
        SUBSTR(HEX(pool_custom_field_groups.uuid), 21)
      )),
      pool_custom_field_groups.model,
      pool_custom_field_groups.name
    FROM pool_custom_field_groups
      %s
    ORDER BY pool_custom_field_groups.position ASC
    |sql}
    where
;;

let find_request =
  let open Caqti_request.Infix in
  {sql|
      WHERE pool_custom_field_groups.uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
  |> select_sql
  |> Caqti_type.string ->! Repo_entity_group.t
;;

let find pool id =
  let open Utils.Lwt_result.Infix in
  Database.find_opt pool find_request (Pool_common.Id.value id)
  ||> CCOption.to_result Pool_message.(Error.NotFound Field.CustomFieldGroup)
;;

let find_by_model_request =
  let open Caqti_request.Infix in
  {sql|
  WHERE pool_custom_field_groups.model = $1
  |sql}
  |> select_sql
  |> Caqti_type.string ->* Repo_entity_group.t
;;

let find_by_model pool model =
  Database.collect pool find_by_model_request (Entity.Model.show model)
;;

let insert_sql =
  {sql|
    INSERT INTO pool_custom_field_groups (
      uuid,
      model,
      name,
      position
    ) VALUES (
      UNHEX(REPLACE($1, '-', '')),
      $2,
      $3,
      (SELECT COUNT(*) FROM pool_custom_field_groups AS g)
    )
  |sql}
;;

let insert_request =
  let open Caqti_request.Infix in
  insert_sql |> Repo_entity_group.t ->. Caqti_type.unit
;;

let insert pool = Database.exec pool insert_request

let update_request =
  let open Caqti_request.Infix in
  {sql|
    UPDATE pool_custom_field_groups
    SET
      name = $2,
      name = $3
    WHERE
      uuid = UNHEX(REPLACE($1, '-', ''))
  |sql}
  |> Repo_entity_group.t ->. Caqti_type.unit
;;

let update pool = Database.exec pool update_request

let destroy_request =
  let open Caqti_request.Infix in
  {sql|
    DELETE FROM pool_custom_field_groups
    WHERE uuid = UNHEX(REPLACE($1, '-', ''))
  |sql}
  |> Caqti_type.(string ->. unit)
;;

let destroy pool m =
  Database.exec pool destroy_request Entity.Group.(m.id |> Id.value)
;;

let update_position_request =
  let open Caqti_request.Infix in
  {sql|
    UPDATE pool_custom_field_groups
      SET
        position = $1
      WHERE uuid = UNHEX(REPLACE($2, '-', ''))
  |sql}
  |> Caqti_type.(t2 int string ->. unit)
;;

let sort_groups pool ids =
  let open Utils.Lwt_result.Infix in
  Lwt_list.mapi_s
    (fun index id ->
      Database.exec pool update_position_request (index, Entity.Id.value id))
    ids
  ||> CCFun.const ()
;;
