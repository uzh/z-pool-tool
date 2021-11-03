module Email = struct
  open Entity.Email
  module RepoModel = Repo_model.Email

  let find_request_sql : type a. a Entity.Email.carrier -> string -> string =
   fun carrier where_fragment ->
    let basic_select = {sql| SELECT |sql} in
    let email_unverified = [ "address"; "token" ] in
    let email_verified = [ "address"; "verified" ] in
    let created_updated_at = [ "created_at"; "updated_at" ] in
    let select fields =
      Format.asprintf
        "%s %s\n%s;"
        basic_select
        (fields @ created_updated_at |> String.concat ",\n")
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
    =
    let where_fragment =
      {sql| WHERE user_users.uuid = UNHEX(REPLACE(?, '-', '')) |sql}
    in
    function
    | UnverifiedC ->
      find_request_sql UnverifiedC where_fragment
      |> Caqti_request.find Caqti_type.string Repo_model.Email.unverified_t
    | VerifiedC ->
      find_request_sql VerifiedC where_fragment
      |> Caqti_request.find Caqti_type.string Repo_model.Email.verified_t
  ;;

  let find db_pool carrier =
    Utils.Database.find
      (Pool_common.Database.Label.value db_pool)
      (find_request carrier)
  ;;

  let insert_request =
    {sql|
      INSERT INTO pool_emails (
        address,
        token,
        created_at,
        updated_at
      ) VALUES (
        $1,
        $2,
        $3,
        $4
      )
    |sql}
    |> Caqti_request.exec Repo_model.Email.unverified_t
  ;;

  let insert dbpool =
    Utils.Database.exec (Pool_common.Database.Label.value dbpool) insert_request
  ;;

  let update_unverified_request =
    {sql|
      UPDATE pool_emails
      SET
        token = $2,
        verified = NULL,
        created_at = $3,
        updated_at = $4
      WHERE address = $1;
    |sql}
    |> Caqti_request.exec Repo_model.Email.unverified_t
  ;;

  let update_verified_request =
    {sql|
      UPDATE pool_emails
      SET
        verified = $2,
        created_at = $3,
        updated_at = $4
      WHERE address = $1;
    |sql}
    |> Caqti_request.exec Repo_model.Email.verified_t
  ;;

  let update
      : type a.
        Pool_common.Database.Label.t
        -> a t
        -> (unit, string) Result.result Lwt.t
    =
   fun db_pool model ->
    let pool = Pool_common.Database.Label.value db_pool in
    match model with
    | Unverified _ as model ->
      Utils.Database.exec pool update_unverified_request model
    | Verified _ as model ->
      Utils.Database.exec pool update_verified_request model
 ;;

  let update_email_request =
    {sql|
      UPDATE pool_emails
      SET
        address = $2,
        token = $3,
        verified = NULL,
        updated_at = $4
      WHERE address = $1;
    |sql}
    |> Caqti_request.exec
         Caqti_type.(
           tup3
             RepoModel.Address.t
             (tup2 RepoModel.Address.t RepoModel.Token.t)
             Pool_common.Repo.UpdatedAt.t)
  ;;

  let update_email db_pool old_email new_email =
    Utils.Database.exec
      (Pool_common.Database.Label.value db_pool)
      update_email_request
      ( address old_email
      , (address new_email, token new_email)
      , Ptime_clock.now () )
  ;;
end
