module RepoEntity = Repo_entity
module Database = Database_pool

module Sql = struct
  let select_from_settings_sql =
    {sql|
      SELECT
         settings_key,
         value,
         created_at,
         updated_at
      FROM
        pool_system_settings
      WHERE
        settings_key = ?
    |sql}
  ;;

  let find_request out_type =
    select_from_settings_sql |> Caqti_request.find Caqti_type.string out_type
  ;;

  let find pool out_type key =
    Utils.Database.find
      (Database.Label.value pool)
      (find_request out_type)
      (key |> Entity.yojson_of_setting_key |> Yojson.Safe.to_string)
  ;;

  let update_sql =
    {sql|
      UPDATE
        pool_system_settings
      SET
        value = ?
      WHERE
        settings_key = ?
    |sql}
  ;;

  let update_request = Caqti_request.exec RepoEntity.Write.t update_sql

  let update pool =
    Utils.Database.exec (Database.Label.value pool) update_request
  ;;
end

let find_languages pool = Sql.find pool RepoEntity.t Entity.Languages
let find_email_suffixes pool = Sql.find pool RepoEntity.t Entity.EmailSuffixes
let find_contact_email pool = Sql.find pool RepoEntity.t Entity.ContactEmail

let find_inactive_user_disable_after pool =
  Sql.find pool RepoEntity.t Entity.InactiveUserDisableAfter
;;

let find_inactive_user_warning pool =
  Sql.find pool RepoEntity.t Entity.InactiveUserWarning
;;

let find_terms_and_conditions pool =
  Sql.find pool RepoEntity.t Entity.TermsAndConditions
;;

let update pool value = Sql.update pool Entity.Write.{ value }
