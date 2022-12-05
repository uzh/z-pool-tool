open Entity
module RepoEntity = Repo_entity
module User = Pool_user

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
  Utils.Database.find_opt
    (Pool_database.Label.value pool)
    (find_by_user_request carrier)
    (Pool_common.Id.value user_id)
  ||> CCOption.to_result Pool_common.Message.(NotFound Field.Email)
;;

let find_by_address pool carrier address =
  let open Utils.Lwt_result.Infix in
  Utils.Database.find_opt
    (Database.Label.value pool)
    (find_by_address_request carrier)
    (address |> User.EmailAddress.value)
  ||> CCOption.to_result Pool_common.Message.(NotFound Field.Email)
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
       tup3 User.Repo.EmailAddress.t Pool_common.Repo.Id.t RepoEntity.Token.t
       ->. unit)
;;

let insert pool t =
  Utils.Database.exec
    (Database.Label.value pool)
    insert_request
    ( address t |> Pool_user.EmailAddress.value
    , user_id t
    , token t |> Token.value )
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
       tup3
         Pool_common.Repo.Id.t
         User.Repo.EmailAddress.t
         RepoEntity.VerifiedAt.t
       ->. unit)
;;

let verify pool t =
  Utils.Database.exec
    (Database.Label.value pool)
    verify_request
    ( user_id t
    , address t |> Pool_user.EmailAddress.value
    , VerifiedAt.create_now () )
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
  Utils.Database.exec
    (Pool_database.Label.value pool)
    delete_unverified_by_user_request
  @@ Pool_common.Id.value id
;;

let delete_email_template_request =
  let open Caqti_request.Infix in
  {sql| DELETE FROM email_templates WHERE label = $1 AND language = $2 |sql}
  |> Caqti_type.(tup2 string string ->. unit)
;;

let delete_email_template pool label language =
  Utils.Database.exec
    (Pool_database.Label.value pool)
    delete_email_template_request
  @@ (TemplateLabel.show label, Pool_common.Language.show language)
;;
