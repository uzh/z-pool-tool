module Database = Pool_database
module Dynparam = Utils.Database.Dynparam

let select_sql =
  Format.asprintf
    {sql|
    SELECT
      LOWER(CONCAT(
        SUBSTR(HEX(pool_custom_field_options.custom_field_option_uuid), 1, 8), '-',
        SUBSTR(HEX(pool_custom_field_options.custom_field_option_uuid), 9, 4), '-',
        SUBSTR(HEX(pool_custom_field_options.custom_field_option_uuid), 13, 4), '-',
        SUBSTR(HEX(pool_custom_field_options.custom_field_option_uuid), 17, 4), '-',
        SUBSTR(HEX(pool_custom_field_options.custom_field_option_uuid), 21)
      )),
      LOWER(CONCAT(
        SUBSTR(HEX(pool_custom_field_options.uuid), 1, 8), '-',
        SUBSTR(HEX(pool_custom_field_options.uuid), 9, 4), '-',
        SUBSTR(HEX(pool_custom_field_options.uuid), 13, 4), '-',
        SUBSTR(HEX(pool_custom_field_options.uuid), 17, 4), '-',
        SUBSTR(HEX(pool_custom_field_options.uuid), 21)
      )),
      pool_custom_field_options.name
    FROM pool_custom_field_options
    %s
     ORDER BY pool_custom_field_options.position ASC
  |sql}
;;

let find_by_multiple_fields_request ids =
  let where =
    Format.asprintf
      {sql|
      WHERE pool_custom_field_options.custom_field_option_uuid in ( %s )
    |sql}
      (CCList.mapi
         (fun i _ -> Format.asprintf "UNHEX(REPLACE($%n, '-', ''))" (i + 1))
         ids
      |> CCString.concat ",")
  in
  select_sql where
;;

let find_by_multiple_fields pool ids =
  if CCList.is_empty ids
  then Lwt.return []
  else
    let open Caqti_request.Infix in
    let dyn =
      CCList.fold_left
        (fun dyn id ->
          dyn |> Dynparam.add Caqti_type.string (id |> Pool_common.Id.value))
        Dynparam.empty
        ids
    in
    let (Dynparam.Pack (pt, pv)) = dyn in
    let request =
      find_by_multiple_fields_request ids |> pt ->* Repo_entity.Option.t
    in
    Utils.Database.collect (pool |> Pool_database.Label.value) request pv
;;

let find_by_field_request =
  let open Caqti_request.Infix in
  {sql|
      WHERE pool_custom_field_options.custom_field_option_uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
  |> select_sql
  |> Caqti_type.string ->* Repo_entity.Option.t
;;

let find_by_field pool id =
  Utils.Database.collect
    (Pool_database.Label.value pool)
    find_by_field_request
    (Pool_common.Id.value id)
;;

let find_multiple_request ids =
  let where =
    Format.asprintf
      {sql|
      WHERE pool_custom_field_options.uuid in ( %s )
    |sql}
      (CCList.mapi
         (fun i _ -> Format.asprintf "UNHEX(REPLACE($%n, '-', ''))" (i + 1))
         ids
      |> CCString.concat ",")
  in
  select_sql where
;;

let find_multiple pool ids =
  if CCList.is_empty ids
  then Lwt.return []
  else
    let open Caqti_request.Infix in
    let dyn =
      CCList.fold_left
        (fun dyn id ->
          dyn |> Dynparam.add Caqti_type.string (id |> Pool_common.Id.value))
        Dynparam.empty
        ids
    in
    let (Dynparam.Pack (pt, pv)) = dyn in
    let request = find_multiple_request ids |> pt ->* Repo_entity.Option.t in
    Utils.Database.collect (pool |> Pool_database.Label.value) request pv
;;

let find_request =
  let open Caqti_request.Infix in
  {sql|
      WHERE pool_custom_field_options.uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
  |> select_sql
  |> Caqti_type.string ->! Repo_entity.Option.t
;;

let find pool id =
  let open Utils.Lwt_result.Infix in
  Utils.Database.find_opt
    (Pool_database.Label.value pool)
    find_request
    (Pool_common.Id.value id)
  ||> CCOption.to_result Pool_common.Message.(NotFound Field.CustomFieldOption)
  >|= Repo_entity.Option.to_entity
;;

let insert_sql =
  {sql|
    INSERT INTO pool_custom_field_options (
      uuid,
      name,
      custom_field_option_uuid
    ) VALUES (
      UNHEX(REPLACE($2, '-', '')),
      $3,
      UNHEX(REPLACE($1, '-', ''))
    )
  |sql}
;;

let insert_request =
  let open Caqti_request.Infix in
  insert_sql |> Repo_entity.Option.t ->. Caqti_type.unit
;;

let insert pool custom_field_id m =
  Utils.Database.exec
    (Database.Label.value pool)
    insert_request
    (Repo_entity.Option.of_entity custom_field_id m)
;;

let update_request =
  let open Caqti_request.Infix in
  {sql|
    UPDATE pool_custom_field_options
    SET
      name = $2
    WHERE
      uuid = UNHEX(REPLACE($1, '-', ''))
  |sql}
  |> Repo_entity.Option.Write.t ->. Caqti_type.unit
;;

let update pool = Utils.Database.exec (Database.Label.value pool) update_request

let destroy_request =
  let open Caqti_request.Infix in
  {sql|
    DELETE FROM pool_custom_field_options
    WHERE uuid = UNHEX(REPLACE($1, '-', ''))
  |sql}
  |> Caqti_type.(string ->. unit)
;;

let destroy pool m =
  Utils.Database.exec
    (Pool_database.Label.value pool)
    destroy_request
    Entity.SelectOption.(m.id |> Id.value)
;;

let update_position_request =
  let open Caqti_request.Infix in
  {sql|
    UPDATE pool_custom_field_options
      SET
        position = $1
      WHERE uuid = UNHEX(REPLACE($2, '-', ''))
  |sql}
  |> Caqti_type.(tup2 int string ->. Caqti_type.unit)
;;

let sort_options pool ids =
  let open Lwt.Infix in
  Lwt_list.mapi_s
    (fun index id ->
      Utils.Database.exec
        (Database.Label.value pool)
        update_position_request
        (index, Entity.Id.value id))
    ids
  >|= CCFun.const ()
;;
