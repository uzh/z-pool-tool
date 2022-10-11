module Database = Pool_database

let get_options pool to_entity field_type id m =
  let open Lwt.Infix in
  (if Entity.FieldType.(equal Select (field_type m))
  then Repo_option.find_by_field pool (id m)
  else [] |> Lwt.return)
  >|= fun options -> to_entity options m
;;

let get_options_of_multiple pool to_entity field_type id fields =
  let open Lwt.Infix in
  fields
  |> CCList.filter_map (fun m ->
       if Entity.FieldType.(equal Select (field_type m))
       then Some (id m)
       else None)
  |> Repo_option.find_by_multiple_fields pool
  >|= fun options -> fields |> CCList.map (to_entity options)
;;

let get_field_type m = m.Repo_entity.field_type
let get_id m = m.Repo_entity.id

module Sql = struct
  let select_sql =
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
        pool_custom_fields.admin_hint,
        pool_custom_fields.admin_overwrite,
        pool_custom_fields.admin_view_only,
        pool_custom_fields.admin_input_only
      FROM pool_custom_fields
    |sql}
  ;;

  let find_all_request =
    let open Caqti_request.Infix in
    select_sql |> Caqti_type.unit ->* Repo_entity.t
  ;;

  let find_all pool () =
    let open Lwt.Infix in
    Utils.Database.collect (Database.Label.value pool) find_all_request ()
    >>= get_options_of_multiple pool Repo_entity.to_entity get_field_type get_id
  ;;

  let find_by_model_request =
    let open Caqti_request.Infix in
    Format.asprintf
      {sql|
    %s
    WHERE pool_custom_fields.model = $1
    |sql}
      select_sql
    |> Caqti_type.string ->* Repo_entity.t
  ;;

  let find_by_model pool model =
    let open Lwt.Infix in
    Utils.Database.collect
      (Database.Label.value pool)
      find_by_model_request
      (Entity.Model.show model)
    >>= get_options_of_multiple pool Repo_entity.to_entity get_field_type get_id
  ;;

  let find_request =
    let open Caqti_request.Infix in
    Format.asprintf
      {sql|
        %s
        WHERE pool_custom_fields.uuid = UNHEX(REPLACE(?, '-', ''))
      |sql}
      select_sql
    |> Caqti_type.string ->! Repo_entity.t
  ;;

  let find pool id =
    let open Utils.Lwt_result.Infix in
    Utils.Database.find_opt
      (Database.Label.value pool)
      find_request
      (id |> Entity.Id.value)
    ||> CCOption.to_result Pool_common.Message.(NotFound Field.CustomField)
    |>> get_options pool Repo_entity.to_entity get_field_type get_id
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
        admin_hint = $9,
        admin_overwrite = $10,
        admin_view_only = $11,
        admin_input_only = $12
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
end

let find_all = Sql.find_all
let find_by_model = Sql.find_by_model
let find = Sql.find
let insert = Sql.insert
let update = Sql.update
