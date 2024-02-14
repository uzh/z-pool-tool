module Dynparam = Utils.Database.Dynparam
module RepoEntity = Repo_entity

module Sql = struct
  open Caqti_request.Infix

  let sql_select_columns =
    Pool_user.Repo.sql_select_columns
    @ [ "pool_admins.email_verified"; "pool_admins.import_pending" ]
  ;;

  let joins =
    {sql|
      LEFT JOIN user_users
        ON pool_admins.user_uuid = user_users.uuid
    |sql}
  ;;

  let insert_request =
    {sql|
      INSERT INTO pool_admins (
        user_uuid,
        email_verified,
        import_pending
      ) VALUES (
        UNHEX(REPLACE($1, '-', '')),
        $2,
        $3
      )
    |sql}
    |> Caqti_type.(RepoEntity.Write.t ->. unit)
  ;;

  let insert pool t =
    Utils.Database.exec
      (Pool_database.Label.value pool)
      insert_request
      (RepoEntity.Write.of_entity t)
  ;;

  let find_request_sql ?(count = false) where_fragment =
    let columns =
      if count then "COUNT(*)" else sql_select_columns |> CCString.concat ", "
    in
    Format.asprintf
      {sql|SELECT %s FROM pool_admins %s %s|sql}
      columns
      joins
      where_fragment
  ;;

  let find_request caqti_type =
    {sql|
      WHERE
        user_users.uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> find_request_sql
    |> Pool_common.Repo.Id.t ->! caqti_type
  ;;

  let find pool id =
    let open Lwt.Infix in
    Utils.Database.find_opt
      (Pool_database.Label.value pool)
      (find_request RepoEntity.t)
      id
    >|= CCOption.to_result Pool_common.Message.(NotFound Field.Admin)
  ;;

  let find_by_email_request caqti_type =
    {sql|
      WHERE
        user_users.email = ?
    |sql}
    |> find_request_sql
    |> Pool_user.Repo.EmailAddress.t ->! caqti_type
  ;;

  let find_by_email pool email =
    let open Lwt.Infix in
    Utils.Database.find_opt
      (Pool_database.Label.value pool)
      (find_by_email_request RepoEntity.t)
      email
    >|= CCOption.to_result Pool_common.Message.(NotFound Field.Admin)
  ;;

  let find_all_request =
    {sql|
      WHERE
        user_users.admin = 1
      |sql}
    |> find_request_sql
    |> Caqti_type.unit ->* RepoEntity.t
  ;;

  let find_by ?query pool =
    Query.collect_and_count pool query ~select:find_request_sql RepoEntity.t
  ;;

  let find_multiple_request ids =
    Format.asprintf
      {sql|
        WHERE user_users.uuid IN ( %s )
      |sql}
      (CCList.mapi
         (fun i _ -> Format.asprintf "UNHEX(REPLACE($%n, '-', ''))" (i + 1))
         ids
       |> CCString.concat ",")
    |> find_request_sql
  ;;

  let find_multiple pool ids =
    if CCList.is_empty ids
    then Lwt.return []
    else (
      let (Dynparam.Pack (pt, pv)) =
        CCList.fold_left
          (fun dyn id ->
            dyn |> Dynparam.add Caqti_type.string (id |> Pool_common.Id.value))
          Dynparam.empty
          ids
      in
      let request = find_multiple_request ids |> pt ->* RepoEntity.t in
      Utils.Database.collect (pool |> Pool_database.Label.value) request pv)
  ;;

  let update_request =
    {sql|
      UPDATE
        pool_admins
      SET
        email_verified = $2,
        import_pending = $3
      WHERE
        user_uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> RepoEntity.Write.t ->. Caqti_type.unit
  ;;

  let update pool t =
    Utils.Database.exec
      (Pool_database.Label.value pool)
      update_request
      (RepoEntity.Write.of_entity t)
  ;;

  let update_sign_in_count_request =
    {sql|
      UPDATE
        pool_admins
      SET
        sign_in_count = sign_in_count + 1,
        last_sign_in_at = NOW()
      WHERE
        user_uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> Caqti_type.(string ->. unit)
  ;;

  let update_sign_in_count pool t =
    Utils.Database.exec
      (Pool_database.Label.value pool)
      update_sign_in_count_request
      Entity.(id t |> Id.value)
  ;;

  let promote_contact_insert_contact_to_promoted_request =
    {sql|
      INSERT INTO pool_contacts_promoted (
        user_uuid,
        terms_accepted_at,
        language,
        cell_phone,
        sms_deactivated,
        experiment_type_preference,
        paused,
        disabled,
        verified,
        email_verified,
        secondary_email,
        secondary_email_verified,
        num_invitations,
        num_assignments,
        num_show_ups,
        num_no_shows,
        num_participations,
        firstname_version,
        lastname_version,
        paused_version,
        language_version,
        experiment_type_preference_version,
        sign_in_count,
        last_sign_in_at,
        admin_notes,
        profile_updated_at,
        registration_attempt_notification_sent_at,
        profile_update_triggered_at,
        import_pending,
        smtp_bounces_count,
        created_at,
        updated_at
      )
      SELECT
        user_uuid,
        terms_accepted_at,
        language,
        cell_phone,
        sms_deactivated,
        experiment_type_preference,
        paused,
        disabled,
        verified,
        email_verified,
        secondary_email,
        secondary_email_verified,
        num_invitations,
        num_assignments,
        num_show_ups,
        num_no_shows,
        num_participations,
        firstname_version,
        lastname_version,
        paused_version,
        language_version,
        experiment_type_preference_version,
        sign_in_count,
        last_sign_in_at,
        admin_notes,
        profile_updated_at,
        registration_attempt_notification_sent_at,
        profile_update_triggered_at,
        import_pending,
        smtp_bounces_count,
        created_at,
        updated_at
      FROM pool_contacts
      WHERE user_uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> Pool_common.Repo.Id.t ->. Caqti_type.unit
  ;;

  let promote_contact_insert_admin_request =
    {sql|
      INSERT INTO pool_admins (user_uuid, email_verified, sign_in_count, last_sign_in_at)
      SELECT user_uuid, email_verified, sign_in_count, last_sign_in_at
      FROM pool_contacts
      WHERE user_uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> Pool_common.Repo.Id.t ->. Caqti_type.unit
  ;;

  let promote_contact_set_admin_request =
    {sql|
      UPDATE user_users
      SET admin = 1
      WHERE uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> Pool_common.Repo.Id.t ->. Caqti_type.unit
  ;;

  let promote_contact_delete_contact_request =
    {sql|
      DELETE FROM pool_contacts
      WHERE user_uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> Pool_common.Repo.Id.t ->. Caqti_type.unit
  ;;

  let promote_contact pool id =
    [ promote_contact_insert_contact_to_promoted_request, id
    ; promote_contact_insert_admin_request, id
    ; promote_contact_set_admin_request, id
    ; promote_contact_delete_contact_request, id
    ]
    |> CCList.map (fun (request, input) connection ->
      let (module Connection : Caqti_lwt.CONNECTION) = connection in
      Connection.exec request input)
    |> Utils.Database.exec_as_transaction (Pool_database.Label.value pool)
  ;;

  let search_by_name_request ?conditions limit =
    let default_contidion =
      {sql|
        (user_users.email LIKE ? OR CONCAT(user_users.given_name, ' ', user_users.name, ' ', user_users.given_name) LIKE ? )
      |sql}
    in
    let where =
      CCOption.map_or
        ~default:default_contidion
        (Format.asprintf "%s AND %s" default_contidion)
        conditions
    in
    Format.asprintf
      "SELECT %s FROM pool_admins %s WHERE %s LIMIT %i"
      (sql_select_columns |> CCString.concat ", ")
      joins
      where
      limit
  ;;

  let search_by_name ?(dyn = Dynparam.empty) ?exclude ?(limit = 20) pool query =
    let open Caqti_request.Infix in
    let exclude_ids =
      Utils.Database.exclude_ids "pool_admins.uuid" Entity.Id.value
    in
    let add_query = Dynparam.add Caqti_type.string ("%" ^ query ^ "%") in
    let dyn = dyn |> add_query |> add_query in
    let dyn, exclude =
      exclude |> CCOption.map_or ~default:(dyn, None) (exclude_ids dyn)
    in
    let conditions =
      [ exclude ]
      |> CCList.filter_map CCFun.id
      |> function
      | [] -> None
      | conditions -> conditions |> CCString.concat " AND " |> CCOption.return
    in
    let (Dynparam.Pack (pt, pv)) = dyn in
    let request =
      search_by_name_request ?conditions limit |> pt ->* RepoEntity.t
    in
    Utils.Database.collect (pool |> Pool_database.Label.value) request pv
  ;;
end

let insert = Sql.insert
let find = Sql.find
let find_by_email = Sql.find_by_email
let find_by = Sql.find_by
let find_multiple = Sql.find_multiple
let update = Sql.update
let update_sign_in_count = Sql.update_sign_in_count
let promote_contact = Sql.promote_contact
