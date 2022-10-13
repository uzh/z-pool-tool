module Database = Pool_database

let to_entity pool to_entity field_type id m =
  let open Lwt.Infix in
  (if Entity.FieldType.(equal Select (field_type m))
  then Repo_option.find_by_field pool (id m)
  else [] |> Lwt.return)
  >|= fun options -> to_entity options m
;;

let get_options_of_multiple pool field_type id fields =
  fields
  |> CCList.filter_map (fun m ->
       if Entity.FieldType.(equal Select (field_type m))
       then Some (id m)
       else None)
  |> Repo_option.find_by_multiple_fields pool
;;

let multiple_to_entity pool to_entity field_type id fields =
  let open Lwt.Infix in
  get_options_of_multiple pool field_type id fields
  >|= fun options -> fields |> CCList.map (to_entity options)
;;

let get_field_type m = m.Repo_entity.field_type
let get_id m = m.Repo_entity.id

module Sql = struct
  let default_order = "ORDER BY pool_custom_fields.position ASC"

  let order_by_group =
    {sql|
      ORDER BY pool_custom_fields.custom_field_group_uuid DESC, pool_custom_fields.position ASC
    |sql}
  ;;

  let select_sql ?(order_by = default_order) where =
    Format.asprintf
      {sql|
      SELECT
        LOWER(CONCAT(
          SUBSTR(HEX(pool_custom_fields.uuid), 1, 8), '-',
          SUBSTR(HEX(pool_custom_fields.uuid), 9, 4), '-',
          SUBSTR(HEX(pool_custom_fields.uuid), 13, 4), '-',
          SUBSTR(HEX(pool_custom_fields.uuid), 17, 4), '-',
          SUBSTR(HEX(pool_custom_fields.uuid), 21)
        )),
        pool_custom_fields.model,
        pool_custom_fields.name,
        pool_custom_fields.hint,
        pool_custom_fields.field_type,
        pool_custom_fields.validation,
        pool_custom_fields.required,
        pool_custom_fields.disabled,
        LOWER(CONCAT(
          SUBSTR(HEX(pool_custom_fields.custom_field_group_uuid), 1, 8), '-',
          SUBSTR(HEX(pool_custom_fields.custom_field_group_uuid), 9, 4), '-',
          SUBSTR(HEX(pool_custom_fields.custom_field_group_uuid), 13, 4), '-',
          SUBSTR(HEX(pool_custom_fields.custom_field_group_uuid), 17, 4), '-',
          SUBSTR(HEX(pool_custom_fields.custom_field_group_uuid), 21)
        )),
        pool_custom_fields.admin_hint,
        pool_custom_fields.admin_overwrite,
        pool_custom_fields.admin_view_only,
        pool_custom_fields.admin_input_only
      FROM pool_custom_fields
      %s
      %s
    |sql}
      where
      order_by
  ;;

  let find_all_request =
    let open Caqti_request.Infix in
    ""
    |> select_sql ~order_by:order_by_group
    |> Caqti_type.unit ->* Repo_entity.t
  ;;

  let find_all pool () =
    let open Lwt.Infix in
    Utils.Database.collect (Database.Label.value pool) find_all_request ()
    >>= multiple_to_entity pool Repo_entity.to_entity get_field_type get_id
  ;;

  let find_by_model_request =
    let open Caqti_request.Infix in
    {sql| WHERE pool_custom_fields.model = $1 |sql}
    |> select_sql ~order_by:order_by_group
    |> Caqti_type.string ->* Repo_entity.t
  ;;

  let find_by_model pool model =
    let open Lwt.Infix in
    Utils.Database.collect
      (Database.Label.value pool)
      find_by_model_request
      (Entity.Model.show model)
    >>= multiple_to_entity pool Repo_entity.to_entity get_field_type get_id
  ;;

  let find_by_group_request =
    let open Caqti_request.Infix in
    {sql| WHERE pool_custom_fields.custom_field_group_uuid = UNHEX(REPLACE(?, '-', '')) |sql}
    |> select_sql
    |> Caqti_type.string ->* Repo_entity.t
  ;;

  let find_by_group pool group =
    let open Lwt.Infix in
    Utils.Database.collect
      (Database.Label.value pool)
      find_by_group_request
      (group |> Entity.Group.Id.value)
    >>= multiple_to_entity pool Repo_entity.to_entity get_field_type get_id
  ;;

  let find_request =
    let open Caqti_request.Infix in
    {sql| WHERE pool_custom_fields.uuid = UNHEX(REPLACE(?, '-', '')) |sql}
    |> select_sql
    |> Caqti_type.string ->! Repo_entity.t
  ;;

  let find pool id =
    let open Utils.Lwt_result.Infix in
    Utils.Database.find_opt
      (Database.Label.value pool)
      find_request
      (id |> Entity.Id.value)
    ||> CCOption.to_result Pool_common.Message.(NotFound Field.CustomField)
    |>> to_entity pool Repo_entity.to_entity get_field_type get_id
  ;;

  let insert_sql =
    {sql|
      INSERT INTO pool_custom_fields (
        uuid,
        model,
        name,
        hint,
        field_type,
        validation,
        required,
        disabled,
        custom_field_group_uuid,
        admin_hint,
        admin_overwrite,
        admin_view_only,
        admin_input_only
      ) VALUES (
        UNHEX(REPLACE(?, '-', '')),
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        UNHEX(REPLACE(?, '-', '')),
        ?,
        ?,
        ?,
        ?
      )
    |sql}
  ;;

  let insert_request =
    let open Caqti_request.Infix in
    insert_sql |> Repo_entity.Write.t ->. Caqti_type.unit
  ;;

  let insert pool t =
    Utils.Database.exec
      (Database.Label.value pool)
      insert_request
      (t |> Repo_entity.Write.of_entity)
  ;;

  let update_request =
    let open Caqti_request.Infix in
    {sql|
      UPDATE pool_custom_fields
      SET
        model = $2,
        name = $3,
        hint = $4,
        field_type = $5,
        validation = $6,
        required = $7,
        disabled = $8,
        custom_field_group_uuid = UNHEX(REPLACE($9, '-', '')),
        admin_hint = $10,
        admin_overwrite = $11,
        admin_view_only = $12,
        admin_input_only = $13
      WHERE
        uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> Repo_entity.Write.t ->. Caqti_type.unit
  ;;

  let update pool t =
    Utils.Database.exec
      (Database.Label.value pool)
      update_request
      (t |> Repo_entity.Write.of_entity)
  ;;

  let update_position_request =
    let open Caqti_request.Infix in
    {sql|
      UPDATE pool_custom_fields
        SET
          position = $1
        WHERE uuid = UNHEX(REPLACE($2, '-', ''))
    |sql}
    |> Caqti_type.(tup2 int string ->. Caqti_type.unit)
  ;;
end

let find_all = Sql.find_all
let find_by_model = Sql.find_by_model
let find_by_group = Sql.find_by_group
let find = Sql.find
let insert = Sql.insert
let update = Sql.update

let sort_fields pool ids =
  let open Lwt.Infix in
  Lwt_list.mapi_s
    (fun index id ->
      Utils.Database.exec
        (Database.Label.value pool)
        Sql.update_position_request
        (index, Entity.Id.value id))
    ids
  >|= CCFun.const ()
;;
