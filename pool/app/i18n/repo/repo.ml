module RepoEntity = Repo_entity

module Sql = struct
  let select_from_i18n_sql where_fragment =
    let select_from =
      {sql|
        SELECT
          LOWER(CONCAT(
            SUBSTR(HEX(uuid), 1, 8), '-',
            SUBSTR(HEX(uuid), 9, 4), '-',
            SUBSTR(HEX(uuid), 13, 4), '-',
            SUBSTR(HEX(uuid), 17, 4), '-',
            SUBSTR(HEX(uuid), 21)
          )),
          i18n_key,
          language,
          content
        FROM pool_i18n
      |sql}
    in
    Format.asprintf "%s %s" select_from where_fragment
  ;;

  let find_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> select_from_i18n_sql
    |> Pool_common.Repo.Id.t ->! RepoEntity.t
  ;;

  let find pool = Database.find pool find_request

  let find_with_default_content_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> select_from_i18n_sql
    |> Pool_common.Repo.Id.t ->! RepoEntity.t_with_default_content
  ;;

  let find_with_default_content pool =
    Database.find pool find_with_default_content_request
  ;;

  let find_by_key_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE i18n_key = ? AND language = ? AND content IS NOT NULL AND content != ''
    |sql}
    |> select_from_i18n_sql
    |> Caqti_type.(t2 RepoEntity.Key.t Pool_common.Repo.Language.t)
       ->! RepoEntity.t
  ;;

  let find_by_key pool key language =
    Database.find pool find_by_key_request (key, language)
  ;;

  let find_by_key_opt pool key language =
    Database.find_opt pool find_by_key_request (key, language)
  ;;

  let find_all_request =
    let open Caqti_request.Infix in
    ""
    |> select_from_i18n_sql
    |> Caqti_type.unit ->* RepoEntity.t_with_default_content
  ;;

  let find_all pool = Database.collect pool find_all_request

  let insert_sql =
    {sql|
      INSERT INTO pool_i18n (
        uuid,
        i18n_key,
        language,
        content
      ) VALUES (
        UNHEX(REPLACE(?, '-', '')),
        ?,
        ?,
        ?
      )
    |sql}
  ;;

  let insert_request =
    let open Caqti_request.Infix in
    insert_sql |> RepoEntity.t ->. Caqti_type.unit
  ;;

  let insert pool = Database.exec pool insert_request

  let update_request =
    let open Caqti_request.Infix in
    {sql|
      UPDATE pool_i18n
      SET
        i18n_key = $2,
        language = $3,
        content = $4
      WHERE
        uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> RepoEntity.t ->. Caqti_type.unit
  ;;

  let update pool = Database.exec pool update_request

  let delete_by_key_request =
    let open Caqti_request.Infix in
    {sql|
      DELETE FROM pool_i18n
      WHERE i18n_key = ?
    |sql}
    |> RepoEntity.Key.t ->. Caqti_type.unit
  ;;

  let delete_by_key pool = Database.exec pool delete_by_key_request

  let terms_and_conditions_last_updated_request =
    let open Caqti_request.Infix in
    {sql|
        SELECT
          updated_at
        FROM
          pool_i18n
        WHERE
          i18n_key = ?
        ORDER BY
          pool_i18n.updated_at DESC
        LIMIT 1
      |sql}
    |> Caqti_type.(string ->! ptime)
  ;;

  let terms_and_conditions_last_updated database_label =
    Database.find
      database_label
      terms_and_conditions_last_updated_request
      Entity.Key.(show TermsAndConditions)
  ;;
end

let find = Sql.find
let find_with_default_content = Sql.find_with_default_content
let find_by_key = Sql.find_by_key
let find_by_key_opt = Sql.find_by_key_opt
let find_all = Sql.find_all
let insert = Sql.insert
let update = Sql.update
let delete_by_key = Sql.delete_by_key
let terms_and_conditions_last_updated = Sql.terms_and_conditions_last_updated
