module RepoEntity = Repo_entity
module Database = Pool_common.Database

let stringify_key = function
  | `Languages -> "languages"
  | `ContactEmail -> "email_contact"
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
end

let find_languages pool () = Sql.find pool RepoEntity.Language.t `Languages

let find_email_suffixes pool () =
  Sql.find pool RepoEntity.EmailSuffix.t `EmailSuffixes
;;
