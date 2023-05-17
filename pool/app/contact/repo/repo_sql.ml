module Id = Pool_common.Id
module Database = Pool_database
module Dynparam = Utils.Database.Dynparam

let select_fields =
  {sql|
    SELECT
      LOWER(CONCAT(
        SUBSTR(HEX(user_users.uuid), 1, 8), '-',
        SUBSTR(HEX(user_users.uuid), 9, 4), '-',
        SUBSTR(HEX(user_users.uuid), 13, 4), '-',
        SUBSTR(HEX(user_users.uuid), 17, 4), '-',
        SUBSTR(HEX(user_users.uuid), 21)
      )),
      user_users.email,
      user_users.username,
      user_users.name,
      user_users.given_name,
      user_users.password,
      user_users.status,
      user_users.admin,
      user_users.confirmed,
      user_users.created_at,
      user_users.updated_at,
      pool_contacts.terms_accepted_at,
      pool_contacts.language,
      pool_contacts.experiment_type_preference,
      pool_contacts.phone_number,
      pool_contacts.paused,
      pool_contacts.disabled,
      pool_contacts.verified,
      pool_contacts.email_verified,
      pool_contacts.num_invitations,
      pool_contacts.num_assignments,
      pool_contacts.num_show_ups,
      pool_contacts.num_no_shows,
      pool_contacts.num_participations,
      pool_contacts.firstname_version,
      pool_contacts.lastname_version,
      pool_contacts.paused_version,
      pool_contacts.language_version,
      pool_contacts.experiment_type_preference_version,
      pool_contacts.created_at,
      pool_contacts.updated_at
    FROM pool_contacts
      LEFT JOIN user_users
      ON pool_contacts.user_uuid = user_users.uuid
    |sql}
;;

let find_request_sql where_fragment =
  Format.asprintf "%s\n%s" select_fields where_fragment
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
  |> Caqti_type.string ->! Repo_model.t
;;

let find pool id =
  let open Utils.Lwt_result.Infix in
  Utils.Database.find_opt
    (Database.Label.value pool)
    find_request
    (Pool_common.Id.value id)
  ||> CCOption.to_result Pool_common.Message.(NotFound Field.Contact)
;;

let find_by_email_request =
  let open Caqti_request.Infix in
  find_request_sql
    {sql|
      WHERE user_users.email = ?
        AND user_users.admin = 0
    |sql}
  |> Caqti_type.string ->! Repo_model.t
;;

let find_by_email pool email =
  let open Utils.Lwt_result.Infix in
  Utils.Database.find_opt
    (Database.Label.value pool)
    find_by_email_request
    (Pool_user.EmailAddress.value email)
  ||> CCOption.to_result Pool_common.Message.(NotFound Field.Contact)
;;

let find_confirmed_request =
  let open Caqti_request.Infix in
  find_request_sql
    {sql|
      WHERE user_users.email = ?
        AND user_users.admin = 0
        AND user_users.confirmed = 1
    |sql}
  |> Caqti_type.string ->! Repo_model.t
;;

let find_confirmed pool email =
  let open Utils.Lwt_result.Infix in
  Utils.Database.find_opt
    (Database.Label.value pool)
    find_confirmed_request
    (Pool_user.EmailAddress.value email)
  ||> CCOption.to_result Pool_common.Message.(NotFound Field.Contact)
;;

let find_multiple_request ids =
  Format.asprintf
    {sql|
      WHERE user_uuid IN ( %s )
      AND user_users.admin = 0
    |sql}
    (CCList.mapi
       (fun i _ -> Format.asprintf "UNHEX(REPLACE($%n, '-', ''))" (i + 1))
       ids
     |> CCString.concat ",")
  |> find_request_sql
;;

let find_multiple pool ids =
  let open Caqti_request.Infix in
  let dyn =
    CCList.fold_left
      (fun dyn id ->
        dyn |> Dynparam.add Caqti_type.string (id |> Pool_common.Id.value))
      Dynparam.empty
      ids
  in
  let (Dynparam.Pack (pt, pv)) = dyn in
  let request = find_multiple_request ids |> pt ->* Repo_model.t in
  Utils.Database.collect (pool |> Pool_database.Label.value) request pv
;;

let find_all_request =
  let open Caqti_request.Infix in
  find_request_sql "" |> Caqti_type.unit ->* Repo_model.t
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

let find_all pool ?query () =
  Query.collect_and_count
    pool
    query
    ~select:find_request_sql
    ~count:select_count
    Repo_model.t
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
        $20
      )
    |sql}
  |> Repo_model.contact ->. Caqti_type.unit
;;

let insert pool = Utils.Database.exec (Database.Label.value pool) insert_request

let update_request =
  let open Caqti_request.Infix in
  {sql|
      UPDATE
        pool_contacts
      SET
        terms_accepted_at = $2,
        language = $3,
        experiment_type_preference = $4,
        phone_number = $5,
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
        experiment_type_preference_version = $19
      WHERE
        user_uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
  |> Repo_model.Write.t ->. Caqti_type.unit
;;

let update pool t =
  Utils.Database.exec
    (Database.Label.value pool)
    update_request
    (Entity.Write.create t)
;;

let delete_unverified_contact_request =
  let open Caqti_request.Infix in
  {sql|
    DELETE FROM pool_contacts
    WHERE user_uuid = UNHEX(REPLACE(?, '-', '')) AND verified IS NULL
  |sql}
  |> Caqti_type.(string ->. unit)
;;

let delete_unverified_user_request =
  let open Caqti_request.Infix in
  {sql|
    DELETE FROM user_users
    WHERE uuid = UNHEX(REPLACE(?, '-', ''))
  |sql}
  |> Caqti_type.(string ->. unit)
;;

let delete_unverified_email_verifications_request =
  let open Caqti_request.Infix in
  {sql|
    DELETE FROM pool_email_verifications
    WHERE user_uuid = UNHEX(REPLACE(?, '-', '')) AND verified IS NULL
  |sql}
  |> Caqti_type.(string ->. unit)
;;

let delete_unverified pool id =
  let exec request =
    Utils.Database.exec
      (Pool_database.Label.value pool)
      request
      (Pool_common.Id.value id)
  in
  let%lwt _ = exec delete_unverified_user_request in
  let%lwt _ = exec delete_unverified_email_verifications_request in
  exec delete_unverified_contact_request
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
        DAY)
    AND
      pool_contacts.profile_update_triggered_at IS NULL
      OR
      (pool_contacts.profile_update_triggered_at <= DATE_SUB(NOW(), INTERVAL
        (SELECT value FROM pool_system_settings WHERE settings_key = $1)
        DAY))
    |sql}
  |> find_request_sql
  |> Caqti_type.(string) ->* Repo_model.t
;;

let find_to_trigger_profile_update pool =
  let settings_key = Settings.trigger_profile_update_after_key_yojson in
  Lwt_result.ok
  @@ Utils.Database.collect
       (Database.Label.value pool)
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
    (CCList.mapi
       (fun i _ -> Format.asprintf "UNHEX(REPLACE($%n, '-', ''))" (i + 2))
       ids
     |> CCString.concat ",")
;;

let update_profile_updated_triggered pool ids =
  let open Caqti_request.Infix in
  let dyn =
    CCList.fold_left
      (fun dyn id ->
        dyn |> Dynparam.add Caqti_type.string (id |> Pool_common.Id.value))
      (Dynparam.empty |> Dynparam.add Caqti_type.ptime (Ptime_clock.now ()))
      ids
  in
  let (Dynparam.Pack (pt, pv)) = dyn in
  let request =
    update_profile_updated_triggered_request ids
    |> (pt ->. Caqti_type.unit) ~oneshot:true
  in
  Utils.Database.exec (pool |> Pool_database.Label.value) request pv
;;

let should_send_registration_attempt_notification_request =
  let open Caqti_request.Infix in
  {|
    SELECT 1
    FROM pool_contacts
    WHERE user_uuid = UNHEX(REPLACE(?, '-', ''))
    AND registration_attempt_notification_sent_at >= DATE_SUB(NOW(), INTERVAL ? SECOND)
  |}
  |> Caqti_type.(tup2 string int ->? int)
;;

let should_send_registration_attempt_notification pool contact =
  let send_notification_again_after = 900 in
  Utils.Database.find_opt
    (Database.Label.value pool)
    should_send_registration_attempt_notification_request
    (Entity.(id contact |> Id.value), send_notification_again_after)
  |> Lwt.map CCOption.is_none
;;

let set_registration_attempt_notification_sent_at_request =
  let open Caqti_request.Infix in
  {|
    UPDATE pool_contacts
    SET registration_attempt_notification_sent_at = NOW()
    WHERE user_uuid = UNHEX(REPLACE(?, '-', ''))
  |}
  |> Caqti_type.(string ->. unit)
;;

let set_registration_attempt_notification_sent_at pool t =
  Utils.Database.exec
    (Database.Label.value pool)
    set_registration_attempt_notification_sent_at_request
    Entity.(id t |> Id.value)
;;

let add_phone_number_request =
  let open Caqti_request.Infix in
  {sql|
    INSERT INTO pool_phone_number_verifications (
      phone_number,
      user_uuid,
      token
    ) VALUES (
      $1,
      UNHEX(REPLACE($2, '-', '')),
      $3
    )
    ON DUPLICATE KEY UPDATE
      phone_number = VALUES(phone_number),
      token = VALUES(token),
      updated_at = NOW(),
      created_at = NOW()
    |sql}
  |> Caqti_type.(tup3 string string string ->. unit)
;;

let add_phone_number pool contact phone_number token =
  Utils.Database.exec
    (Pool_database.Label.value pool)
    add_phone_number_request
    ( Pool_user.PhoneNumber.value phone_number
    , Entity.(id contact |> Id.value)
    , Pool_common.Token.value token )
;;

let phone_number_verifiaction_sql ?(where = "") () =
  Format.asprintf
    {sql|
    SELECT
      phone_number,
      created_at
    FROM pool_phone_number_verifications
    WHERE user_uuid = UNHEX(REPLACE(?, '-', ''))
    AND created_at >= (DATE(NOW()) - INTERVAL 1 DAY)
    %s
    LIMIT 1
    |sql}
    where
;;

let find_phone_number_verification_by_contact_request =
  let open Caqti_request.Infix in
  phone_number_verifiaction_sql ()
  |> Caqti_type.(string ->? Pool_user.Repo.UnverifiedPhoneNumber.t)
;;

let find_phone_number_verification_by_contact pool contact =
  Utils.Database.find_opt
    (Pool_database.Label.value pool)
    find_phone_number_verification_by_contact_request
    Entity.(id contact |> Id.value)
;;

let find_phone_number_verification_by_contact_and_token_request =
  let open Caqti_request.Infix in
  phone_number_verifiaction_sql ~where:"AND token = ?" ()
  |> Caqti_type.(tup2 string string ->? Pool_user.Repo.UnverifiedPhoneNumber.t)
;;

let find_phone_number_verification_by_contact_and_token pool contact token =
  let open Utils.Lwt_result.Infix in
  Utils.Database.find_opt
    (Pool_database.Label.value pool)
    find_phone_number_verification_by_contact_and_token_request
    (Entity.(id contact |> Id.value), Pool_common.Token.value token)
  ||> CCOption.to_result Pool_common.Message.(Invalid Field.Token)
;;

let find_full_phone_number_verification_by_contact_request =
  let open Caqti_request.Infix in
  {sql|
    SELECT
      phone_number,
      token,
      created_at
    FROM pool_phone_number_verifications
    WHERE user_uuid = UNHEX(REPLACE(?, '-', ''))
    LIMIT 1
    |sql}
  |> Caqti_type.(string ->? Pool_user.Repo.UnverifiedPhoneNumber.full)
;;

let find_full_phone_number_verification_by_contact pool contact =
  let open Utils.Lwt_result.Infix in
  Utils.Database.find_opt
    (Pool_database.Label.value pool)
    find_full_phone_number_verification_by_contact_request
    Entity.(id contact |> Id.value)
  ||> CCOption.to_result Pool_common.Message.(NotFound Field.Token)
;;

let delete_unverified_phone_number_requeset =
  let open Caqti_request.Infix in
  {sql|
    DELETE FROM pool_phone_number_verifications
    WHERE user_uuid = UNHEX(REPLACE(?, '-', ''))
  |sql}
  |> Caqti_type.(string ->. unit)
;;

let delete_unverified_phone_number pool contact =
  Utils.Database.exec
    (Pool_database.Label.value pool)
    delete_unverified_phone_number_requeset
    Entity.(id contact |> Id.value)
;;
