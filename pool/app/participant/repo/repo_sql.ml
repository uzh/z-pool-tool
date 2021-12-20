module Id = Pool_common.Id
module Database = Pool_database

let find_request_sql where_fragment =
  Format.asprintf
    "%s\n%s;"
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
        pool_participants.recruitment_channel,
        pool_participants.terms_accepted_at,
        pool_participants.paused,
        pool_participants.disabled,
        pool_participants.verified,
        pool_participants.firstname_version,
        pool_participants.lastname_version,
        pool_participants.paused_version,
        pool_participants.created_at,
        pool_participants.updated_at
      FROM pool_participants
        LEFT JOIN user_users
        ON pool_participants.user_uuid = user_users.uuid
    |sql}
    where_fragment
;;

let find_request =
  find_request_sql
    {sql|
      WHERE user_users.uuid = UNHEX(REPLACE(?, '-', ''))
        AND user_users.admin = 0
    |sql}
  |> Caqti_request.find Caqti_type.string Repo_model.t
;;

let find pool id =
  let open Lwt.Infix in
  Utils.Database.find_opt
    (Database.Label.value pool)
    find_request
    (Pool_common.Id.value id)
  >|= CCOption.to_result Pool_common.Message.(NotFound Participant)
;;

let find_by_email_request =
  find_request_sql
    {sql|
      WHERE user_users.email = ?
        AND user_users.admin = 0
    |sql}
  |> Caqti_request.find Caqti_type.string Repo_model.t
;;

let find_by_email pool email =
  let open Lwt.Infix in
  Utils.Database.find_opt
    (Database.Label.value pool)
    find_by_email_request
    (Pool_user.EmailAddress.value email)
  >|= CCOption.to_result Pool_common.Message.(NotFound Participant)
;;

let find_confirmed_request =
  find_request_sql
    {sql|
      WHERE user_users.email = ?
        AND user_users.admin = 0
        AND user_users.confirmed = 1
    |sql}
  |> Caqti_request.find Caqti_type.string Repo_model.t
;;

let find_confirmed pool email =
  let open Lwt.Infix in
  Utils.Database.find_opt
    (Database.Label.value pool)
    find_confirmed_request
    (Pool_user.EmailAddress.value email)
  >|= CCOption.to_result Pool_common.Message.(NotFound Participant)
;;

let insert_request =
  Caqti_request.exec
    Repo_model.participant
    {sql|
      INSERT INTO pool_participants (
        user_uuid,
        recruitment_channel,
        terms_accepted_at,
        paused,
        disabled,
        verified,
        firstname_version,
        lastname_version,
        paused_version,
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
        $11
      )
    |sql}
;;

let insert pool = Utils.Database.exec (Database.Label.value pool) insert_request

let update_paused_request =
  let open Pool_common.Repo in
  {sql|
    UPDATE pool_participants
    SET
      paused = $2,
      paused_version = $3
    WHERE user_uuid = UNHEX(REPLACE($1, '-', ''));
  |sql}
  |> Caqti_request.exec Caqti_type.(tup3 Id.t Pool_user.Repo.Paused.t Version.t)
;;

let update_paused pool (Entity.{ paused; paused_version; _ } as participant) =
  Utils.Database.exec
    (Database.Label.value pool)
    update_paused_request
    ( participant |> Entity.id |> Id.value
    , paused |> Pool_user.Paused.value
    , paused_version |> Pool_common.Version.value )
;;

let update_version_for_request field =
  let field =
    match field with
    | `Firstname -> "firstname_version"
    | `Lastname -> "lastname_version"
  in
  let update = {sql| UPDATE pool_participants SET |sql} in
  let where = {sql| WHERE user_uuid = UNHEX(REPLACE($1, '-', '')); |sql} in
  Format.asprintf "%s\n%s = $2\n%s" update field where
  |> Caqti_request.exec Caqti_type.(Pool_common.Repo.(tup2 Id.t Version.t))
;;

let update_version_for pool field (id, version) =
  Utils.Database.exec
    (Database.Label.value pool)
    (field |> update_version_for_request)
    (id |> Id.value, version |> Pool_common.Version.value)
;;

let update_request =
  Caqti_request.exec
    Repo_model.Write.t
    {sql|
      UPDATE
        pool_participants
      SET
        recruitment_channel = $2,
        terms_accepted_at = $3,
        paused = $4,
        disabled = $5,
        verified = $6,
        firstname_version = $7,
        lastname_version = $8,
        paused_version = $9
      WHERE
        user_uuid = UNHEX(REPLACE($1, '-', ''));
    |sql}
;;

let update pool t =
  Utils.Database.exec
    (Database.Label.value pool)
    update_request
    (Entity.Write.create t)
;;

let delete_unverified_participant_request =
  {sql|
    DELETE FROM pool_participants
    WHERE user_uuid = UNHEX(REPLACE(?, '-', '')) AND verified IS NULL
  |sql}
  |> Caqti_request.exec Caqti_type.string
;;

let delete_unverified_user_request =
  {sql|
    DELETE FROM user_users
    WHERE uuid = UNHEX(REPLACE(?, '-', ''))
  |sql}
  |> Caqti_request.exec Caqti_type.string
;;

let delete_unverified_email_verifications_request =
  {sql|
    DELETE FROM pool_email_verifications
    WHERE user_uuid = UNHEX(REPLACE(?, '-', '')) AND verified IS NULL;
  |sql}
  |> Caqti_request.exec Caqti_type.string
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
  exec delete_unverified_participant_request
;;
