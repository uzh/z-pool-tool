open Entity
module RepoEntity = Repo_entity
module User = Pool_user

let find_request_sql : type a. a carrier -> string -> string =
 fun carrier where_fragment ->
  let basic_select = {sql| SELECT |sql} in
  let user_id =
    {sql|
    LOWER(CONCAT(
      SUBSTR(HEX(sihl_user_uuid), 1, 8), '-',
      SUBSTR(HEX(sihl_user_uuid), 9, 4), '-',
      SUBSTR(HEX(sihl_user_uuid), 13, 4), '-',
      SUBSTR(HEX(sihl_user_uuid), 17, 4), '-',
      SUBSTR(HEX(sihl_user_uuid), 21)
    ))
    |sql}
  in
  let basic_fields = [ "address"; user_id ] in
  let email_unverified = [ "token" ] in
  let email_verified = [ "verified" ] in
  let created_updated_at = [ "created_at"; "updated_at" ] in
  let from_fragment = {sql| FROM pool_email_verifications |sql} in
  let select fields =
    Format.asprintf
      "%s %s\n%s\n%s;"
      basic_select
      (basic_fields @ fields @ created_updated_at |> CCString.concat ", ")
      from_fragment
      where_fragment
  in
  match carrier with
  | UnverifiedC -> select email_unverified
  | VerifiedC -> select email_verified
;;

let find_by_user_request
    : type a.
      a carrier
      -> (string, a t, [< `Many | `One | `Zero > `One ]) Caqti_request.t
  = function
  | UnverifiedC ->
    find_request_sql
      UnverifiedC
      {sql| WHERE sihl_user_uuid = UNHEX(REPLACE(?, '-', '')) AND verified IS NULL ORDER BY created_at DESC LIMIT 1 |sql}
    |> Caqti_request.find Caqti_type.string RepoEntity.unverified_t
  | VerifiedC ->
    find_request_sql
      VerifiedC
      {sql| WHERE sihl_user_uuid = UNHEX(REPLACE(?, '-', '')) AND verified IS NOT NULL ORDER BY created_at DESC LIMIT 1 |sql}
    |> Caqti_request.find Caqti_type.string RepoEntity.verified_t
;;

let find_by_address_request
    : type a.
      a carrier
      -> (string, a t, [< `Many | `One | `Zero > `One ]) Caqti_request.t
  = function
  | UnverifiedC ->
    find_request_sql
      UnverifiedC
      {sql| WHERE address = ? AND verified IS NULL ORDER BY created_at DESC LIMIT 1 |sql}
    |> Caqti_request.find Caqti_type.string RepoEntity.unverified_t
  | VerifiedC ->
    find_request_sql
      VerifiedC
      {sql| WHERE address = ? AND verified IS NOT NULL ORDER BY created_at DESC LIMIT 1 |sql}
    |> Caqti_request.find Caqti_type.string RepoEntity.verified_t
;;

let find_by_user pool carrier user_id =
  let open Lwt.Infix in
  Utils.Database.find_opt
    (Pool_database.Label.value pool)
    (find_by_user_request carrier)
    (Pool_common.Id.value user_id)
  >|= CCOption.to_result Pool_common.Message.(NotFound Email)
;;

let find_by_address pool carrier address =
  let open Lwt.Infix in
  Utils.Database.find_opt
    (Database.Label.value pool)
    (find_by_address_request carrier)
    (address |> User.EmailAddress.value)
  >|= CCOption.to_result Pool_common.Message.(NotFound Email)
;;

let insert_request =
  {sql|
      INSERT INTO pool_email_verifications (
        address,
        sihl_user_uuid,
        token,
        created_at,
        updated_at
      ) VALUES (
        $1,
        UNHEX(REPLACE($2, '-', '')),
        $3,
        $4,
        $5
      )
    |sql}
  |> Caqti_request.exec RepoEntity.unverified_t
;;

let insert pool = Utils.Database.exec (Database.Label.value pool) insert_request

let verify_request =
  {sql|
      UPDATE pool_email_verifications
      SET
        verified = $3
      WHERE sihl_user_uuid = UNHEX(REPLACE($1, '-', '')) AND address = $2 AND verified IS NULL;
    |sql}
  |> Caqti_request.exec
       Caqti_type.(
         tup3
           Pool_common.Repo.Id.t
           User.Repo.EmailAddress.t
           RepoEntity.VerifiedAt.t)
;;

let verify pool t =
  Utils.Database.exec
    (Database.Label.value pool)
    verify_request
    ( user_id t |> Pool_common.Id.value
    , address t |> Pool_user.EmailAddress.value
    , VerifiedAt.create_now () )
;;

let delete_unverified_by_user_request =
  {sql|
    DELETE FROM pool_email_verifications
    WHERE sihl_user_uuid = UNHEX(REPLACE(?, '-', '')) AND verified IS NULL;
  |sql}
  |> Caqti_request.exec Caqti_type.string
;;

let delete_unverified_by_user pool id =
  Utils.Database.exec
    (Pool_database.Label.value pool)
    delete_unverified_by_user_request
  @@ Pool_common.Id.value id
;;
