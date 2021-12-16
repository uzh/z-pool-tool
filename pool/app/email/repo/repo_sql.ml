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

let find_request
    : type a.
      a carrier
      -> (string, a t, [< `Many | `One | `Zero > `One ]) Caqti_request.t
  = function
  | UnverifiedC ->
    find_request_sql
      UnverifiedC
      {sql| WHERE address = ? AND verified IS NULL |sql}
    |> Caqti_request.find Caqti_type.string RepoEntity.unverified_t
  | VerifiedC ->
    find_request_sql
      VerifiedC
      {sql| WHERE address = ? AND verified IS NOT NULL |sql}
    |> Caqti_request.find Caqti_type.string RepoEntity.verified_t
;;

let find pool carrier address =
  let open Lwt.Infix in
  Utils.Database.find_opt
    (Database.Label.value pool)
    (find_request carrier)
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

let update_unverified_request =
  {sql|
      UPDATE pool_email_verifications
      SET
        token = $2,
        verified = NULL
      WHERE address = $1;
    |sql}
  |> Caqti_request.exec
       Caqti_type.(tup2 User.Repo.EmailAddress.t RepoEntity.Token.t)
;;

let update_verified_request =
  {sql|
      UPDATE pool_email_verifications
      SET
        verified = $2
      WHERE address = $1;
    |sql}
  |> Caqti_request.exec
       Caqti_type.(tup2 User.Repo.EmailAddress.t RepoEntity.VerifiedAt.t)
;;

let update : type a. Database.Label.t -> a t -> unit Lwt.t =
 fun pool model ->
  let pool = Database.Label.value pool in
  match model with
  | Unverified { address; token; _ } ->
    Utils.Database.exec
      pool
      update_unverified_request
      (address |> User.EmailAddress.value, token |> Token.value)
  | Verified { address; verified_at; _ } ->
    Utils.Database.exec
      pool
      update_verified_request
      (address |> User.EmailAddress.value, verified_at |> VerifiedAt.value)
;;

let update_email_request =
  {sql|
      UPDATE pool_email_verifications
      SET
        address = $2,
        token = $3,
        verified = NULL
      WHERE address = $1;
    |sql}
  |> Caqti_request.exec
       Caqti_type.(
         tup2
           User.Repo.EmailAddress.t
           (tup2 User.Repo.EmailAddress.t RepoEntity.Token.t))
;;

let update_email pool old_email new_email =
  Utils.Database.exec
    (Database.Label.value pool)
    update_email_request
    ( address old_email |> Pool_user.EmailAddress.value
    , (address new_email |> Pool_user.EmailAddress.value, token new_email) )
;;

let delete_request =
  {sql|
      DELETE FROM pool_email_verifications
      WHERE address = ?;
    |sql}
  |> Caqti_request.exec Caqti_type.string
;;

let delete pool email =
  Utils.Database.exec (Database.Label.value pool) delete_request
  @@ (address email |> Pool_user.EmailAddress.value)
;;
