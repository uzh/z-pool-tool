module RepoEntity = Repo_entity
module Database = Pool_common.Database

let stringify_key = function
  | `Languages -> "languages"
  | `ContactEmail -> "contact_email"
  | `EmailSuffixes -> "email_suffixes"
  | `UserSetToInactiveAfter -> "user_set_to_inactive_after"
  | `UserSendWarningBeforeInactive -> "user_send_warning_before_inactive"
;;

module Sql = struct
  let select_from_settings_sql where_fragment =
    let select_from =
      {sql|
      SELECT
         value,
         created_at,
         updated_at
      FROM pool_system_settings |sql}
    in
    Format.asprintf "%s %s" select_from where_fragment
  ;;

  let where_fragment = {sql| WHERE settings_key = ? |sql}

  let find_request caqti =
    where_fragment
    |> select_from_settings_sql
    |> Caqti_request.find Caqti_type.string caqti
  ;;

  let find pool caqti key =
    Utils.Database.find
      (Database.Label.value pool)
      (find_request caqti)
      (key |> stringify_key)
  ;;

  let update_sql where_fragment =
    let update_fragment =
      {sql|
      UPDATE
        pool_system_settings
      SET
        value = ?
    |sql}
    in
    Format.asprintf "%s %s" update_fragment where_fragment
  ;;

  let update_request caqti =
    where_fragment |> update_sql |> Caqti_request.exec caqti
  ;;

  let update pool caqti =
    Utils.Database.exec
      (Pool_common.Database.Label.value pool)
      (update_request caqti)
  ;;
end

let find_languages pool () =
  Sql.find pool RepoEntity.TenantLanguages.t `Languages
;;

let update_languages pool values =
  Sql.update
    pool
    Caqti_type.(tup2 RepoEntity.TenantLanguages.Values.t string)
    (values, `Languages |> stringify_key)
;;

let find_email_suffixes pool () =
  Sql.find pool RepoEntity.TenantEmailSuffixes.t `EmailSuffixes
;;

let find_contact_email pool () =
  Sql.find pool RepoEntity.TenantContactEmail.t `ContactEmail
;;
