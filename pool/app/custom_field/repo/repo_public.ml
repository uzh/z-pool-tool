module Database = Pool_database

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
        pool_custom_fields.name,
        pool_custom_fields.hint,
        pool_custom_fields.field_type,
        pool_custom_fields.validation,
        pool_custom_fields.required,
        pool_custom_field_answers.uuid,
        pool_custom_field_answers.answer,
        pool_custom_field_answers.version
      FROM pool_custom_fields
      LEFT JOIN pool_custom_field_answers
        ON pool_custom_field_answers.custom_field_uuid = pool_custom_fields.uuid
        AND pool_custom_field_answers.entity_uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
  ;;

  let find_all_for_contact_request =
    let open Caqti_request.Infix in
    Format.asprintf
      {sql|%s
      WHERE pool_custom_fields.model = $2
      AND disabled = 0
    |sql}
      select_sql
    |> Caqti_type.(tup2 string string ->* Repo_entity.Public.t)
  ;;

  let find_all_for_contact pool id =
    Utils.Database.collect
      (Pool_database.Label.value pool)
      find_all_for_contact_request
      (Pool_common.Id.value id, Entity.Model.(show Contact))
  ;;
end

let find_all_for_contact = Sql.find_all_for_contact
