module Id = Pool_common.Id

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
    |sql}
  |> Caqti_request.find Caqti_type.string Repo_model.t
;;

let find db_pool id =
  Utils.Database.find
    (Pool_common.Database.Label.value db_pool)
    find_request
    (Pool_common.Id.value id)
;;

let find_by_email_request =
  find_request_sql {sql|
    WHERE user_users.email = ?
  |sql}
  |> Caqti_request.find Caqti_type.string Repo_model.t
;;

let find_by_email db_pool email =
  Utils.Database.find
    (Pool_common.Database.Label.value db_pool)
    find_by_email_request
    (Common_user.Email.Address.value email)
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
        $8
      )
    |sql}
;;

let insert db_pool =
  Utils.Database.exec (Pool_common.Database.Label.value db_pool) insert_request
;;

let update_request =
  Caqti_request.exec
    Repo_model.participant
    {sql|
      UPDATE pool_participants
      SET
        recruitment_channel = $2,
        terms_accepted_at = $3,
        paused = $4,
        disabled = $5,
        verified = $6,
        created_at = $7,
        updated_at = $8
      WHERE user_uuid = UNHEX(REPLACE($1, '-', ''));
    |sql}
;;

let update db_pool =
  Utils.Database.exec (Pool_common.Database.Label.value db_pool) update_request
;;
