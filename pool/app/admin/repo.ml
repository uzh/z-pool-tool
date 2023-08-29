module Dynparam = Utils.Database.Dynparam
module RepoEntity = Repo_entity

module Sql = struct
  open Caqti_request.Infix

  let insert_request =
    {sql|
      INSERT INTO pool_admins (
        user_uuid,
        import_pending
      ) VALUES (
        UNHEX(REPLACE($1, '-', '')),
        $2
      )
    |sql}
    |> Caqti_type.(
         tup2 Pool_common.Repo.Id.t Pool_user.Repo.ImportPending.t ->. unit)
  ;;

  let insert pool t =
    Utils.Database.exec
      (Pool_database.Label.value pool)
      insert_request
      (Entity.id t, t.Entity.import_pending)
  ;;

  let select_from_admin_columns =
    Format.asprintf
      {sql|
        %s
        pool_admins.import_pending
      |sql}
      Pool_user.Repo.select_from_sihl_user_columns
  ;;

  let select_from_users_sql ?order_by where_fragment =
    let select_from =
      Format.asprintf
        {sql|
          SELECT
          %s
          FROM user_users
          INNER JOIN pool_admins
          ON pool_admins.user_uuid = user_users.uuid
        |sql}
        select_from_admin_columns
    in
    let query = Format.asprintf "%s %s" select_from where_fragment in
    match order_by with
    | Some order_by -> Format.asprintf "%s ORDER BY %s" query order_by
    | None -> query
  ;;

  let select_imported_admins_sql ~import_columns ~where ~limit =
    Format.asprintf
      {sql|
        SELECT
        %s,
        %s
        FROM user_users
        INNER JOIN pool_admins
          ON pool_admins.user_uuid = user_users.uuid
        INNER JOIN pool_user_imports
          ON user_users.uuid = pool_user_imports.user_uuid
        WHERE
          %s
        ORDER BY
          pool_admins.created_at ASC
        LIMIT %i
      |sql}
      select_from_admin_columns
      import_columns
      where
      limit
  ;;

  let find_request caqti_type =
    {sql|
      WHERE user_users.uuid = UNHEX(REPLACE(?, '-', ''))
      AND user_users.confirmed = 1
    |sql}
    |> select_from_users_sql
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

  let find_all_request =
    {sql|
      WHERE
      user_users.confirmed = 1
      AND
      user_users.admin = 1
      |sql}
    |> select_from_users_sql
    |> Caqti_type.unit ->* RepoEntity.t
  ;;

  let find_all pool =
    Utils.Database.collect (Pool_database.Label.value pool) find_all_request
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
    |> select_from_users_sql
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
        import_pending = $2
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

  let promote_contact_insert_admin_request =
    {sql|
      INSERT INTO pool_admins (user_uuid, sign_in_count, last_sign_in_at)
      SELECT user_uuid, sign_in_count, last_sign_in_at
      FROM pool_contacts
      WHERE user_uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> Pool_common.Repo.Id.t ->. Caqti_type.unit
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
end

let insert = Sql.insert
let find = Sql.find
let find_all = Sql.find_all
let find_multiple = Sql.find_multiple
let update = Sql.update
let update_sign_in_count = Sql.update_sign_in_count
let promote_contact = Sql.promote_contact
