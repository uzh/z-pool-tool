module RepoEntity = Repo_entity
module Database = Pool_database

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
    let open Caqti_request.Infix in
    select_from_settings_sql |> Caqti_type.string ->! out_type
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

  let update_request =
    let open Caqti_request.Infix in
    update_sql |> RepoEntity.Write.t ->. Caqti_type.unit
  ;;

  let update pool =
    Utils.Database.exec (Database.Label.value pool) update_request
  ;;

  let upsert_request =
    let open Caqti_request.Infix in
    {sql|
      INSERT INTO pool_system_settings (
        uuid,
        settings_key,
        value,
        created_at,
        updated_at
      ) VALUES (
        UNHEX(REPLACE(?, '-', '')),
        ?,
        ?,
        ?,
        ?
      ) ON DUPLICATE KEY UPDATE
        id = id
    |sql}
    |> Caqti_type.(tup2 Pool_common.Repo.Id.t RepoEntity.t ->. unit)
  ;;

  let upsert pool =
    Utils.Database.exec (Database.Label.value pool) upsert_request
  ;;

  let delete_request =
    let open Caqti_request.Infix in
    {sql|
      DELETE FROM pool_system_settings
      WHERE settings_key = ?
    |sql}
    |> Caqti_type.(string ->. unit)
  ;;

  let delete pool =
    Utils.Database.exec (Database.Label.value pool) delete_request
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

let find_trigger_profile_update_after pool =
  Sql.find pool RepoEntity.t Entity.TriggerProfileUpdateAfter
;;

let find_terms_and_conditions pool =
  Sql.find pool RepoEntity.t Entity.TermsAndConditions
;;

let find_default_reminder_lead_time pool =
  Sql.find pool RepoEntity.t Entity.ReminderLeadTime
;;

let find_default_text_msg_reminder_lead_time pool =
  Sql.find pool RepoEntity.t Entity.TextMsgReminderLeadTime
;;

let update pool value = Sql.update pool Entity.Write.{ value }

let upsert pool ?(id = Pool_common.Id.create ()) (value : Entity.Value.t) =
  Sql.upsert
    pool
    ( id
    , { Entity.value
      ; created_at = Pool_common.CreatedAt.create ()
      ; updated_at = Pool_common.UpdatedAt.create ()
      } )
;;

let delete pool key =
  Sql.delete pool (key |> Entity.yojson_of_setting_key |> Yojson.Safe.to_string)
;;
