module Database = Database

let has_options field_type m =
  Entity.FieldType.(equal Select (field_type m) || equal MultiSelect (field_type m))
;;

let to_entity pool to_entity field_type id m =
  let open Utils.Lwt_result.Infix in
  (if has_options field_type m
   then Repo_option.find_by_field pool (id m)
   else [] |> Lwt.return)
  ||> fun options -> to_entity options m
;;

let get_options_of_multiple pool field_type id fields =
  fields
  |> CCList.filter_map (fun m -> if has_options field_type m then Some (id m) else None)
  |> Repo_option.find_by_multiple_fields pool
;;

let multiple_to_entity pool to_entity field_type id fields =
  let open Utils.Lwt_result.Infix in
  get_options_of_multiple pool field_type id fields
  ||> fun options -> fields |> CCList.map (to_entity options)
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
        pool_custom_fields.admin_override,
        pool_custom_fields.admin_view_only,
        pool_custom_fields.admin_input_only,
        pool_custom_fields.prompt_on_registration,
        pool_custom_fields.published_at,
        pool_custom_fields.show_on_session_close_screen,
        pool_custom_fields.show_on_session_detail_screen
      FROM pool_custom_fields
      %s
      %s
    |sql}
      where
      order_by
  ;;

  let find_by_model_request =
    let open Caqti_request.Infix in
    {sql| WHERE pool_custom_fields.model = $1 |sql}
    |> select_sql ~order_by:order_by_group
    |> Caqti_type.string ->* Repo_entity.t
  ;;

  let find_by_model pool model =
    let open Utils.Lwt_result.Infix in
    Database.collect pool find_by_model_request (Entity.Model.show model)
    >|> multiple_to_entity pool Repo_entity.to_entity get_field_type get_id
  ;;

  let find_by_group_request =
    let open Caqti_request.Infix in
    {sql| WHERE pool_custom_fields.custom_field_group_uuid = UNHEX(REPLACE(?, '-', '')) |sql}
    |> select_sql
    |> Caqti_type.string ->* Repo_entity.t
  ;;

  let find_by_group pool group =
    let open Utils.Lwt_result.Infix in
    Database.collect pool find_by_group_request (group |> Entity.Group.Id.value)
    >|> multiple_to_entity pool Repo_entity.to_entity get_field_type get_id
  ;;

  let find_ungrouped_by_model_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE pool_custom_fields.model = $1
        AND pool_custom_fields.custom_field_group_uuid IS NULL
    |sql}
    |> select_sql
    |> Caqti_type.string ->* Repo_entity.t
  ;;

  let find_ungrouped_by_model pool model =
    let open Utils.Lwt_result.Infix in
    Database.collect pool find_ungrouped_by_model_request (Entity.Model.show model)
    >|> multiple_to_entity pool Repo_entity.to_entity get_field_type get_id
  ;;

  let find_request =
    let open Caqti_request.Infix in
    {sql| WHERE pool_custom_fields.uuid = UNHEX(REPLACE(?, '-', '')) |sql}
    |> select_sql
    |> Caqti_type.string ->! Repo_entity.t
  ;;

  let find pool id =
    let open Utils.Lwt_result.Infix in
    Database.find_opt pool find_request (id |> Entity.Id.value)
    ||> CCOption.to_result Pool_message.(Error.NotFound Field.CustomField)
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
        admin_override,
        admin_view_only,
        admin_input_only,
        prompt_on_registration,
        show_on_session_close_screen,
        show_on_session_detail_screen,
        position
      ) VALUES (
        UNHEX(REPLACE($1, '-', '')),
        $2,
        $3,
        $4,
        $5,
        $6,
        $7,
        $8,
        UNHEX(REPLACE($9, '-', '')),
        $10,
        $11,
        $12,
        $13,
        $14,
        $15,
        $16,
        (SELECT
          COUNT(*)
          FROM pool_custom_fields AS f
          WHERE f.model = $2)
        )
    |sql}
  ;;

  let insert_request =
    let open Caqti_request.Infix in
    insert_sql |> Repo_entity.Write.t ->. Caqti_type.unit
  ;;

  let insert pool t = Database.exec pool insert_request (t |> Repo_entity.Write.of_entity)

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
        admin_override = $11,
        admin_view_only = $12,
        admin_input_only = $13,
        prompt_on_registration = $14,
        show_on_session_close_screen = $15,
        show_on_session_detail_screen = $16
      WHERE
        uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> Repo_entity.Write.t ->. Caqti_type.unit
  ;;

  let update pool t = Database.exec pool update_request (t |> Repo_entity.Write.of_entity)

  let publish_request =
    let open Caqti_request.Infix in
    {sql|
      UPDATE pool_custom_fields
      SET
        published_at = NOW()
      WHERE
        uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> Caqti_type.string ->. Caqti_type.unit
  ;;

  let publish pool t =
    let%lwt () = Database.exec pool publish_request (t |> Entity.id |> Entity.Id.value) in
    Repo_option.publish_by_custom_field pool (Entity.id t)
  ;;

  let update_position_request =
    let open Caqti_request.Infix in
    {sql|
      UPDATE pool_custom_fields
        SET
          position = $1
        WHERE uuid = UNHEX(REPLACE($2, '-', ''))
    |sql}
    |> Caqti_type.(t2 int string ->. unit)
  ;;

  let delete_request =
    let open Caqti_request.Infix in
    {sql|
      DELETE FROM pool_custom_fields
      WHERE
        uuid = UNHEX(REPLACE($1, '-', ''))
        AND published_at IS NULL
    |sql}
    |> Caqti_type.(string ->. unit)
  ;;

  let delete pool t =
    let%lwt () = Database.exec pool delete_request (t |> Entity.id |> Entity.Id.value) in
    Repo_option.destroy_by_custom_field pool (Entity.id t)
  ;;

  let table_view_column = function
    | `SessionClose -> "show_on_session_close_screen"
    | `SessionDetail -> "show_on_session_detail_screen"
  ;;

  let find_by_table_view_request table_view =
    let open Caqti_request.Infix in
    Format.asprintf
      {sql|
        WHERE pool_custom_fields.model = $1
        AND %s = 1
      |sql}
      (table_view_column table_view)
    |> select_sql
    |> Caqti_type.string ->* Repo_entity.t
  ;;

  let find_by_table_view pool table_view =
    let open Utils.Lwt_result.Infix in
    Database.collect
      pool
      (find_by_table_view_request table_view)
      Entity.Model.(show Contact)
    >|> multiple_to_entity pool Repo_entity.to_entity get_field_type get_id
  ;;
end

let find_by_model = Sql.find_by_model
let find_by_group = Sql.find_by_group
let find_ungrouped_by_model = Sql.find_ungrouped_by_model
let find = Sql.find
let insert = Sql.insert
let update = Sql.update
let publish = Sql.publish
let delete = Sql.delete

let sort_fields pool ids =
  let open Utils.Lwt_result.Infix in
  Lwt_list.mapi_s
    (fun index id ->
       Database.exec pool Sql.update_position_request (index, Entity.Id.value id))
    ids
  ||> CCFun.const ()
;;
