open CCFun.Infix
open Utils.Lwt_result.Infix
open Repo_entity
module Dynparam = Database.Dynparam

let src = Logs.Src.create "contact.repo"

let make_sql_select_columns ~user_table ~contact_table =
  let with_tablemame = Format.asprintf "%s.%s" contact_table in
  Pool_user.Repo.make_sql_select_columns ~tablename:user_table
  @ ([ "terms_accepted_at"
     ; "language"
     ; "experiment_type_preference"
     ; "cell_phone"
     ; "paused"
     ; "disabled"
     ; "verified"
     ; "email_verified"
     ; "num_invitations"
     ; "num_assignments"
     ; "num_show_ups"
     ; "num_no_shows"
     ; "num_participations"
     ; "firstname_version"
     ; "lastname_version"
     ; "paused_version"
     ; "language_version"
     ; "experiment_type_preference_version"
     ; "import_pending"
     ; "created_at"
     ; "updated_at"
     ]
     |> CCList.map with_tablemame)
;;

let sql_select_columns =
  make_sql_select_columns ~user_table:"user_users" ~contact_table:"pool_contacts"
;;

let joins =
  {sql|
    INNER JOIN user_users
      ON pool_contacts.user_uuid = user_users.uuid
  |sql}
;;

let find_request_sql
      ?(distinct = true)
      ?(additional_joins = [])
      ?(count = false)
      where_fragment
  =
  let columns =
    if count
    then "COUNT(DISTINCT user_users.uuid)"
    else CCString.concat ", " sql_select_columns
  in
  Format.asprintf
    {sql|SELECT %s %s FROM pool_contacts %s %s|sql}
    (if distinct && not count then "DISTINCT" else "")
    columns
    (joins :: additional_joins |> CCString.concat "\n")
    where_fragment
;;

let join_custom_field_answers =
  {sql|
    LEFT JOIN pool_custom_field_answers ON pool_custom_field_answers.entity_uuid = user_users.uuid
  |sql}
;;

let find_request =
  let open Caqti_request.Infix in
  find_request_sql
    {sql|
      WHERE user_users.uuid = UNHEX(REPLACE(?, '-', ''))
        AND user_users.admin = 0
    |sql}
  |> Id.t ->! t
;;

let find pool id =
  Database.find_opt pool find_request id
  ||> CCOption.to_result Pool_message.(Error.NotFound Field.Contact)
;;

let find_admin_comment_request =
  let open Caqti_request.Infix in
  {sql|
    SELECT admin_notes
    FROM pool_contacts
    WHERE pool_contacts.user_uuid = UNHEX(REPLACE(?, '-', ''))
  |sql}
  |> Id.t ->! Caqti_type.option AdminComment.t
;;

let find_admin_comment pool id =
  Database.find_opt pool find_admin_comment_request id ||> CCOption.flatten
;;

let find_by_email_request =
  let open Caqti_request.Infix in
  find_request_sql
    {sql|
      WHERE user_users.email = ?
        AND user_users.admin = 0
    |sql}
  |> Pool_user.Repo.EmailAddress.t ->! t
;;

let find_by_email pool email =
  Database.find_opt pool find_by_email_request email
  ||> CCOption.to_result Pool_message.(Error.NotFound Field.Contact)
;;

let find_confirmed_request =
  let open Caqti_request.Infix in
  find_request_sql
    {sql|
      WHERE user_users.email = ?
        AND user_users.admin = 0
        AND user_users.confirmed = 1
    |sql}
  |> Pool_user.Repo.EmailAddress.t ->! t
;;

let find_confirmed pool email =
  Database.find_opt pool find_confirmed_request email
  ||> CCOption.to_result Pool_message.(Error.NotFound Field.Contact)
;;

let find_multiple_request ids =
  Format.asprintf
    {sql|
      WHERE user_uuid IN ( %s )
      AND user_users.admin = 0
    |sql}
    (CCList.mapi (fun i _ -> Format.asprintf "UNHEX(REPLACE($%n, '-', ''))" (i + 1)) ids
     |> CCString.concat ",")
  |> find_request_sql
;;

let find_multiple pool ids =
  let open Caqti_request.Infix in
  let (Dynparam.Pack (pt, pv)) =
    CCList.fold_left (fun dyn id -> dyn |> Dynparam.add Id.t id) Dynparam.empty ids
  in
  let request = find_multiple_request ids |> pt ->* t in
  Database.collect pool request pv
;;

let find_all_request =
  let open Caqti_request.Infix in
  find_request_sql "" |> Caqti_type.unit ->* t
;;

let select_count where_fragment =
  let select_from =
    {sql|
      SELECT COUNT(*)
      FROM pool_contacts
      LEFT JOIN user_users
        ON pool_contacts.user_uuid = user_users.uuid
    |sql}
  in
  Format.asprintf "%s %s" select_from where_fragment
;;

let list_by_user ?query pool actor =
  let open CCFun.Infix in
  let target_sql = CCFun.(Role.Target.show %> Format.asprintf "'%s'") in
  let targets_to_sql targets = targets |> CCList.map target_sql |> CCString.concat "," in
  (* Ignoring default permission joins *)
  let dyn, sql, _ =
    Guard.Persistence.with_user_permission actor "pool_experiments.uuid" `Contact
  in
  let contact_targets = targets_to_sql [ `Contact; `ContactInfo ] in
  let joins =
    [%string
      {sql|
        LEFT JOIN user_permissions 
          ON pool_contacts.user_uuid = user_permissions.target_uuid
          OR (
            user_permissions.target_uuid IS NULL
            AND user_permissions.target_model IN (%{contact_targets})
          )
    |sql}]
  in
  let join_assignment_with =
    targets_to_sql [ `Assignment; `Session; `Experiment; `Location ]
  in
  let where =
    [%string
      {sql|
        (
          EXISTS (
            SELECT
              1
            FROM
              user_permissions
            WHERE
              (target_model = %{target_sql `Contact} AND target_uuid IS NULL)
              OR target_uuid = pool_contacts.user_uuid
          )
          OR EXISTS (
            SELECT
              1
            FROM
              pool_assignments
              INNER JOIN pool_sessions ON pool_sessions.uuid = pool_assignments.session_uuid
              INNER JOIN user_permissions ON
              (
                -- ASSIGNMENT PERMISSION
                user_permissions.target_uuid = pool_assignments.uuid 

                -- SESSION PERMISSION
                OR user_permissions.target_uuid = pool_sessions.uuid
      
                -- LOCATION PERMISSION
                OR (
                  pool_sessions.location_uuid IS NOT NULL
                  AND pool_sessions.location_uuid = user_permissions.target_uuid
                )

                -- EXPERIMENT PERMISSION
                OR user_permissions.target_uuid = pool_sessions.experiment_uuid

                -- GLOBAL PERMISSIONS
                OR (
                  user_permissions.target_uuid IS NULL
                  AND user_permissions.target_model IN (%{join_assignment_with})
                )
              )
            WHERE
              pool_assignments.contact_uuid = pool_contacts.user_uuid
          )
        )
  |sql}]
  in
  let select ?count =
    find_request_sql ?count ~distinct:true ~additional_joins:[ joins ]
    %> Format.asprintf "%s %s" sql
  in
  Query.collect_and_count pool query ~select ~dyn ~where Repo_entity.t
;;

let all ?query pool =
  let select = find_request_sql ~distinct:true ?additional_joins:None in
  Query.collect_and_count pool query ~select t
;;

let insert_request =
  let open Caqti_request.Infix in
  {sql|
      INSERT INTO pool_contacts (
        user_uuid,
        terms_accepted_at,
        language,
        experiment_type_preference,
        paused,
        disabled,
        verified,
        email_verified,
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
        import_pending,
        created_at,
        updated_at
      ) VALUES (
        UNHEX(REPLACE($1, '-', '')),
        $2,
        $3,
        $4,
        $5,
        $6,
        $7,
        $8,
        $9,
        $10,
        $11,
        $12,
        $13,
        $14,
        $15,
        $16,
        $17,
        $18,
        $19,
        $20,
        $21
      )
    |sql}
  |> contact ->. Caqti_type.unit
;;

let insert pool = Database.exec pool insert_request

let update_request =
  let open Caqti_request.Infix in
  {sql|
      UPDATE
        pool_contacts
      SET
        terms_accepted_at = $2,
        language = $3,
        experiment_type_preference = $4,
        cell_phone = $5,
        paused = $6,
        disabled = $7,
        verified = $8,
        email_verified = $9,
        num_invitations = $10,
        num_assignments = $11,
        num_show_ups = $12,
        num_no_shows = $13,
        num_participations = $14,
        firstname_version = $15,
        lastname_version = $16,
        paused_version = $17,
        language_version = $18,
        experiment_type_preference_version = $19,
        import_pending = $20
      WHERE
        user_uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
  |> Write.t ->. Caqti_type.unit
;;

let update pool = Entity.to_write %> Database.exec pool update_request

let delete_unverified_contact_request =
  let open Caqti_request.Infix in
  {sql|
    DELETE FROM pool_contacts
    WHERE user_uuid = UNHEX(REPLACE(?, '-', '')) AND verified IS NULL
  |sql}
  |> Id.t ->. Caqti_type.unit
;;

let delete_unverified_user_request =
  let open Caqti_request.Infix in
  {sql|
    DELETE FROM user_users
    WHERE uuid = UNHEX(REPLACE(?, '-', ''))
  |sql}
  |> Id.t ->. Caqti_type.unit
;;

let delete_unverified_email_verifications_request =
  let open Caqti_request.Infix in
  {sql|
    DELETE FROM pool_email_verifications
    WHERE user_uuid = UNHEX(REPLACE(?, '-', '')) AND verified IS NULL
  |sql}
  |> Id.t ->. Caqti_type.unit
;;

let delete_unverified pool id =
  let exec request = Database.exec pool request id in
  let%lwt () = exec delete_unverified_contact_request in
  let%lwt () = exec delete_unverified_email_verifications_request in
  exec delete_unverified_user_request
;;

let find_to_trigger_profile_update_request =
  let open Caqti_request.Infix in
  {sql|
    WHERE
      pool_contacts.paused = 0
    AND
      pool_contacts.disabled = 0
    AND
      pool_contacts.email_verified IS NOT NULL
    AND
      pool_contacts.profile_updated_at <= DATE_SUB(NOW(), INTERVAL
        (SELECT value FROM pool_system_settings WHERE settings_key = $1)
        SECOND)
    AND
      pool_contacts.profile_update_triggered_at IS NULL
      OR
      (pool_contacts.profile_update_triggered_at <= DATE_SUB(NOW(), INTERVAL
        (SELECT value FROM pool_system_settings WHERE settings_key = $1)
        SECOND))
    |sql}
  |> find_request_sql
  |> Caqti_type.(string) ->* t
;;

let find_to_trigger_profile_update pool =
  let settings_key = Settings.trigger_profile_update_after_key_yojson in
  Lwt_result.ok
  @@ Database.collect
       pool
       find_to_trigger_profile_update_request
       (settings_key |> Yojson.Safe.to_string)
;;

let update_profile_updated_triggered_request ids =
  Format.asprintf
    {sql|
    UPDATE pool_contacts
      SET
      profile_update_triggered_at = $1
      WHERE user_uuid IN ( %s )
   |sql}
    (CCList.mapi (fun i _ -> Format.asprintf "UNHEX(REPLACE($%n, '-', ''))" (i + 2)) ids
     |> CCString.concat ",")
;;

let update_profile_updated_triggered pool ids =
  let open Caqti_request.Infix in
  let dyn =
    CCList.fold_left
      (fun dyn id -> dyn |> Dynparam.add Id.t id)
      (Dynparam.empty |> Dynparam.add Caqti_type.ptime (Ptime_clock.now ()))
      ids
  in
  let (Dynparam.Pack (pt, pv)) = dyn in
  let request =
    update_profile_updated_triggered_request ids |> (pt ->. Caqti_type.unit) ~oneshot:true
  in
  Database.exec pool request pv
;;

let should_send_registration_attempt_notification_request =
  let open Caqti_request.Infix in
  {|
    SELECT 1
    FROM pool_contacts
    WHERE user_uuid = UNHEX(REPLACE(?, '-', ''))
    AND registration_attempt_notification_sent_at >= DATE_SUB(NOW(), INTERVAL ? SECOND)
  |}
  |> Caqti_type.(t2 Id.t int ->? int)
;;

let should_send_registration_attempt_notification pool contact =
  let send_notification_again_after = 900 in
  Database.find_opt
    pool
    should_send_registration_attempt_notification_request
    (Entity.id contact, send_notification_again_after)
  |> Lwt.map CCOption.is_none
;;

let set_registration_attempt_notification_sent_at_request =
  let open Caqti_request.Infix in
  {|
    UPDATE pool_contacts
    SET registration_attempt_notification_sent_at = NOW()
    WHERE user_uuid = UNHEX(REPLACE(?, '-', ''))
  |}
  |> Id.t ->. Caqti_type.unit
;;

let set_registration_attempt_notification_sent_at pool =
  Entity.id %> Database.exec pool set_registration_attempt_notification_sent_at_request
;;

let add_cell_phone_request =
  let open Caqti_request.Infix in
  {sql|
    INSERT INTO pool_cell_phone_verifications (
      cell_phone,
      user_uuid,
      token
    ) VALUES (
      $1,
      UNHEX(REPLACE($2, '-', '')),
      $3
    )
    ON DUPLICATE KEY UPDATE
      cell_phone = VALUES(cell_phone),
      token = VALUES(token),
      updated_at = NOW(),
      created_at = NOW()
    |sql}
  |> Caqti_type.(
       t3 Pool_user.Repo.CellPhone.t Id.t Pool_common.Repo.VerificationCode.t ->. unit)
;;

let add_cell_phone pool contact cell_phone code =
  Database.exec pool add_cell_phone_request (cell_phone, Entity.(id contact), code)
;;

let cell_phone_verifiaction_sql ?(where = "") () =
  Format.asprintf
    {sql|
    SELECT
      cell_phone,
      created_at
    FROM pool_cell_phone_verifications
    WHERE user_uuid = UNHEX(REPLACE(?, '-', ''))
    AND created_at >= (NOW() - INTERVAL 1 HOUR)
    %s
    LIMIT 1
    |sql}
    where
;;

let find_cell_phone_verification_by_contact_request =
  let open Caqti_request.Infix in
  cell_phone_verifiaction_sql () |> Id.t ->? Pool_user.Repo.UnverifiedCellPhone.t
;;

let find_cell_phone_verification_by_contact pool =
  Entity.id %> Database.find_opt pool find_cell_phone_verification_by_contact_request
;;

let find_cell_phone_verification_by_contact_and_code_request =
  let open Caqti_request.Infix in
  cell_phone_verifiaction_sql ~where:"AND token = ?" ()
  |> Caqti_type.(
       t2 Id.t Pool_common.Repo.VerificationCode.t
       ->? Pool_user.Repo.UnverifiedCellPhone.t)
;;

let find_cell_phone_verification_by_contact_and_code pool contact code =
  Database.find_opt
    pool
    find_cell_phone_verification_by_contact_and_code_request
    (Entity.(id contact), code)
  ||> CCOption.to_result Pool_message.(Error.Invalid Field.Token)
;;

let find_full_cell_phone_verification_by_contact_request =
  let open Caqti_request.Infix in
  {sql|
    SELECT
      cell_phone,
      token,
      created_at
    FROM pool_cell_phone_verifications
    WHERE user_uuid = UNHEX(REPLACE(?, '-', ''))
    LIMIT 1
    |sql}
  |> Id.t ->? Pool_user.Repo.UnverifiedCellPhone.full
;;

let find_full_cell_phone_verification_by_contact pool contact =
  Database.find_opt
    pool
    find_full_cell_phone_verification_by_contact_request
    Entity.(id contact)
  ||> CCOption.to_result Pool_message.(Error.NotFound Field.Token)
;;

let delete_unverified_cell_phone_requeset =
  let open Caqti_request.Infix in
  {sql|
    DELETE FROM pool_cell_phone_verifications
    WHERE user_uuid = UNHEX(REPLACE(?, '-', ''))
  |sql}
  |> Id.t ->. Caqti_type.unit
;;

let delete_unverified_cell_phone pool =
  Entity.id %> Database.exec pool delete_unverified_cell_phone_requeset
;;

let update_sign_in_count_request =
  let open Caqti_request.Infix in
  {sql|
    UPDATE
      pool_contacts
    SET
      sign_in_count = sign_in_count + 1,
      last_sign_in_at = NOW()
    WHERE
      user_uuid = UNHEX(REPLACE($1, '-', ''))
  |sql}
  |> Id.t ->. Caqti_type.unit
;;

let update_sign_in_count pool =
  Entity.id %> Database.exec pool update_sign_in_count_request
;;

let remove_deactivation_notifications pool contact =
  let open Caqti_request.Infix in
  let request =
    {sql|
      DELETE FROM pool_contact_deactivation_notification
      WHERE contact_uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> Id.t ->. Caqti_type.unit
  in
  Database.exec pool request (Entity.id contact)
;;

let set_inactive_request =
  let open Caqti_request.Infix in
  {sql|
    UPDATE
      user_users
    SET
      status = "inactive"
    WHERE
      uuid = UNHEX(REPLACE($1, '-', ''))
  |sql}
  |> Id.t ->. Caqti_type.unit
;;

let set_inactive pool = Entity.id %> Database.exec pool set_inactive_request

let find_last_signin_at pool contact =
  let request =
    let open Caqti_request.Infix in
    {sql|
      SELECT last_sign_in_at
      FROM pool_contacts
      WHERE user_uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> Repo_entity.Id.t ->! Caqti_type.ptime
  in
  contact |> Entity.id |> Database.find pool request
;;

module InactivityNotification = struct
  let insert pool contact =
    let open Caqti_request.Infix in
    let request =
      {sql|
       INSERT INTO pool_contact_deactivation_notification (contact_uuid)
       VALUES (UNHEX(REPLACE(?, '-', '')))
      |sql}
      |> Repo_entity.Id.t ->. Caqti_type.unit
    in
    Database.exec pool request (Entity.id contact)
  ;;
end
