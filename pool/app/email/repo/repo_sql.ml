open Entity
module RepoEntity = Repo_entity
module User = Pool_user

let not_found = Pool_message.(Error.NotFound Field.Smtp)

let find_request_sql : type a. a carrier -> string -> string =
  fun carrier where_fragment ->
  let basic_select = {sql| SELECT |sql} in
  let basic_fields =
    {sql|
      pool_email_verifications.address,
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
      user_users.updated_at
    |sql}
  in
  let email_unverified = "pool_email_verifications.token" in
  let email_verified = "pool_email_verifications.verified" in
  let created_updated_at =
    [ "pool_email_verifications.created_at"
    ; "pool_email_verifications.updated_at"
    ]
  in
  let from_fragment =
    {sql|
      FROM pool_email_verifications
      LEFT JOIN user_users
      ON pool_email_verifications.user_uuid = user_users.uuid
    |sql}
  in
  let select fields =
    Format.asprintf
      "%s %s\n%s\n%s"
      basic_select
      ([ basic_fields; fields ] @ created_updated_at |> CCString.concat ", ")
      from_fragment
      where_fragment
  in
  match carrier with
  | UnverifiedC -> select email_unverified
  | VerifiedC -> select email_verified
;;

let find_by_user_request
  : type a. a carrier -> (string, a t, [ `One ]) Caqti_request.t
  =
  let open Caqti_request.Infix in
  function
  | UnverifiedC ->
    find_request_sql
      UnverifiedC
      {sql| WHERE pool_email_verifications.user_uuid = UNHEX(REPLACE(?, '-', '')) AND pool_email_verifications.verified IS NULL ORDER BY pool_email_verifications.created_at DESC LIMIT 1 |sql}
    |> Caqti_type.string ->! RepoEntity.unverified_t
  | VerifiedC ->
    find_request_sql
      VerifiedC
      {sql| WHERE pool_email_verifications.user_uuid = UNHEX(REPLACE(?, '-', '')) AND pool_email_verifications.verified IS NOT NULL ORDER BY pool_email_verifications.verified DESC LIMIT 1 |sql}
    |> Caqti_type.string ->! RepoEntity.verified_t
;;

let find_by_address_request
  : type a. a carrier -> (string, a t, [ `One ]) Caqti_request.t
  =
  let open Caqti_request.Infix in
  function
  | UnverifiedC ->
    find_request_sql
      UnverifiedC
      {sql| WHERE pool_email_verifications.address = ? AND pool_email_verifications.verified IS NULL ORDER BY pool_email_verifications.created_at DESC LIMIT 1 |sql}
    |> Caqti_type.string ->! RepoEntity.unverified_t
  | VerifiedC ->
    find_request_sql
      VerifiedC
      {sql| WHERE pool_email_verifications.address = ? AND pool_email_verifications.verified IS NOT NULL ORDER BY pool_email_verifications.created_at DESC LIMIT 1 |sql}
    |> Caqti_type.string ->! RepoEntity.verified_t
;;

let find_by_user pool carrier user_id =
  let open Utils.Lwt_result.Infix in
  Database.find_opt
    pool
    (find_by_user_request carrier)
    (Pool_common.Id.value user_id)
  ||> CCOption.to_result Pool_message.(Error.NotFound Field.Email)
;;

let find_by_address pool carrier address =
  let open Utils.Lwt_result.Infix in
  Database.find_opt
    pool
    (find_by_address_request carrier)
    (address |> User.EmailAddress.value)
  ||> CCOption.to_result Pool_message.(Error.NotFound Field.Email)
;;

let insert_request =
  let open Caqti_request.Infix in
  {sql|
      INSERT INTO pool_email_verifications (
        address,
        user_uuid,
        token
      ) VALUES (
        $1,
        UNHEX(REPLACE($2, '-', '')),
        $3
      )
    |sql}
  |> Caqti_type.(
       t3 User.Repo.EmailAddress.t Pool_common.Repo.Id.t RepoEntity.Token.t
       ->. unit)
;;

let insert pool t =
  Database.exec
    pool
    insert_request
    (address t, user_id t, token t |> Token.value)
;;

let verify_request =
  let open Caqti_request.Infix in
  {sql|
      UPDATE pool_email_verifications
      SET
        verified = $3
      WHERE user_uuid = UNHEX(REPLACE($1, '-', '')) AND address = $2 AND verified IS NULL
    |sql}
  |> Caqti_type.(
       t3 Pool_common.Repo.Id.t User.Repo.EmailAddress.t RepoEntity.VerifiedAt.t
       ->. unit)
;;

let verify pool t =
  Database.exec
    pool
    verify_request
    (user_id t, address t, VerifiedAt.create_now ())
;;

let delete_unverified_by_user_request =
  let open Caqti_request.Infix in
  {sql|
    DELETE FROM pool_email_verifications
    WHERE user_uuid = UNHEX(REPLACE(?, '-', '')) AND verified IS NULL
  |sql}
  |> Caqti_type.(string ->. unit)
;;

let delete_unverified_by_user pool id =
  Database.exec pool delete_unverified_by_user_request
  @@ Pool_common.Id.value id
;;

module Smtp = struct
  module Id = Entity_smtp.Id

  let select_smtp_sql ?(with_password = false) where_fragment =
    let with_password_fragment =
      if with_password then {sql|password,|sql} else {sql||sql}
    in
    let select_from =
      Format.asprintf
        {sql|
          SELECT
            LOWER(CONCAT(
              SUBSTR(HEX(uuid), 1, 8), '-',
              SUBSTR(HEX(uuid), 9, 4), '-',
              SUBSTR(HEX(uuid), 13, 4), '-',
              SUBSTR(HEX(uuid), 17, 4), '-',
              SUBSTR(HEX(uuid), 21)
            )),
            label,
            server,
            port,
            username,
            %s
            mechanism,
            protocol,
            default_account
          FROM pool_smtp
        |sql}
        with_password_fragment
    in
    Format.asprintf "%s %s" select_from where_fragment
  ;;

  let find_request =
    let open Caqti_request.Infix in
    {sql|WHERE uuid = UNHEX(REPLACE(?, '-', ''))|sql}
    |> select_smtp_sql
    |> RepoEntity.SmtpAuth.(Id.t ->! t)
  ;;

  let find pool id =
    let open Utils.Lwt_result.Infix in
    Database.find_opt pool find_request id ||> CCOption.to_result not_found
  ;;

  let find_by_label_request =
    let open Caqti_request.Infix in
    {sql|WHERE label = ?|sql}
    |> select_smtp_sql
    |> Caqti_type.string ->! RepoEntity.SmtpAuth.t
  ;;

  let find_by_label pool label =
    Database.find_opt
      pool
      find_by_label_request
      (Entity.SmtpAuth.Label.value label)
  ;;

  let find_full_request =
    let open Caqti_request.Infix in
    {sql|WHERE uuid = UNHEX(REPLACE(?, '-', ''))|sql}
    |> select_smtp_sql ~with_password:true
    |> RepoEntity.SmtpAuth.(Id.t ->! Write.t)
  ;;

  let find_full pool id =
    let open Utils.Lwt_result.Infix in
    Database.find_opt pool find_full_request id ||> CCOption.to_result not_found
  ;;

  let find_full_default_request =
    let open Caqti_request.Infix in
    {sql|WHERE default_account = 1|sql}
    |> select_smtp_sql ~with_password:true
    |> Caqti_type.unit ->! RepoEntity.SmtpAuth.Write.t
  ;;

  let find_full_default pool =
    let open Utils.Lwt_result.Infix in
    Database.find_opt pool find_full_default_request ()
    ||> CCOption.to_result not_found
  ;;

  let find_default_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE default_account = 1
      LIMIT 1
    |sql}
    |> select_smtp_sql
    |> Caqti_type.unit ->! RepoEntity.SmtpAuth.t
  ;;

  let find_default_opt pool = Database.find_opt pool find_default_request ()

  let find_default pool =
    let open Utils.Lwt_result.Infix in
    pool |> find_default_opt ||> CCOption.to_result not_found
  ;;

  let find_all_request =
    let open Caqti_request.Infix in
    "" |> select_smtp_sql |> Caqti_type.unit ->* RepoEntity.SmtpAuth.t
  ;;

  let find_all pool = Database.collect pool find_all_request ()

  let select_count where_fragment =
    Format.asprintf
      {sql|
        SELECT COUNT(*)
        FROM pool_smtp
        %s
      |sql}
      where_fragment
  ;;

  let find_by query pool =
    Query.collect_and_count
      pool
      (Some query)
      ~select:(fun ?(count = false) fragment ->
        if count then select_count fragment else select_smtp_sql fragment)
      RepoEntity.SmtpAuth.t
  ;;

  let unset_default_flags pool =
    let open Caqti_request.Infix in
    let request =
      {sql|
        UPDATE pool_smtp
          SET default_account = 0
      |sql}
      |> Caqti_type.(unit ->. unit)
    in
    Database.exec pool request
  ;;

  let insert_request =
    let open Caqti_request.Infix in
    {sql|
      INSERT INTO pool_smtp (
        uuid,
        label,
        server,
        port,
        username,
        password,
        mechanism,
        protocol,
        default_account
      ) VALUES (
        UNHEX(REPLACE(?, '-', '')),
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        ?
      )
    |sql}
    |> RepoEntity.SmtpAuth.Write.t ->. Caqti_type.unit
  ;;

  let insert pool t =
    let%lwt () =
      match t.SmtpAuth.Write.default with
      | true -> unset_default_flags pool ()
      | false -> Lwt.return ()
    in
    Database.exec pool insert_request t
  ;;

  let update_request =
    let open Caqti_request.Infix in
    {sql|
      UPDATE pool_smtp
      SET
        label = $2,
        server = $3,
        port = $4,
        username = $5,
        mechanism = $6,
        protocol = $7,
        default_account = $8
      WHERE
        uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> RepoEntity.SmtpAuth.t ->. Caqti_type.unit
  ;;

  let update pool t =
    let%lwt () =
      match t.SmtpAuth.default with
      | true -> unset_default_flags pool ()
      | false -> Lwt.return ()
    in
    Database.exec pool update_request t
  ;;

  let delete_request =
    let open Caqti_request.Infix in
    {sql|
        DELETE FROM pool_smtp
        WHERE uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> RepoEntity.SmtpAuth.(Id.t ->. Caqti_type.unit)
  ;;

  let delete pool t = Database.exec pool delete_request t

  let update_password_request =
    let open Caqti_request.Infix in
    {sql|
        UPDATE pool_smtp
        SET
          password = $2
        WHERE
          uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> Caqti_type.(RepoEntity.SmtpAuth.(t2 Id.t (option Password.t)) ->. unit)
  ;;

  let update_password pool = Database.exec pool update_password_request
end
