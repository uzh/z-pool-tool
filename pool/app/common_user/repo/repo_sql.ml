module Email = struct
  open Entity.Email
  module RepoModel = Repo_model.Email

  let find_request_sql : type a. a Entity.Email.carrier -> string -> string =
   fun carrier where_fragment ->
    let basic_select = {sql| SELECT |sql} in
    let email_unverified = [ "address"; "token" ] in
    let email_verified = [ "address"; "verified" ] in
    let created_updated_at = [ "created_at"; "updated_at" ] in
    let from_fragment = {sql| FROM pool_email_verifications |sql} in
    let select fields =
      Format.asprintf
        "%s %s\n%s\n%s;"
        basic_select
        (fields @ created_updated_at |> String.concat ", ")
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
      |> Caqti_request.find Caqti_type.string Repo_model.Email.unverified_t
    | VerifiedC ->
      find_request_sql
        VerifiedC
        {sql| WHERE address = ? AND verified IS NOT NULL |sql}
      |> Caqti_request.find Caqti_type.string Repo_model.Email.verified_t
  ;;

  let find db_pool carrier address =
    let open Lwt.Infix in
    Utils.Database.find_opt
      (Pool_common.Database.Label.value db_pool)
      (find_request carrier)
      address
    >|= CCOpt.to_result Pool_common.Error.(NotFound Email)
  ;;

  let insert_request =
    {sql|
      INSERT INTO pool_email_verifications (
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
      UPDATE pool_email_verifications
      SET
        token = $2,
        verified = NULL
      WHERE address = $1;
    |sql}
    |> Caqti_request.exec
         Caqti_type.(tup2 RepoModel.Address.t RepoModel.Token.t)
  ;;

  let update_verified_request =
    {sql|
      UPDATE pool_email_verifications
      SET
        verified = $2
      WHERE address = $1;
    |sql}
    |> Caqti_request.exec
         Caqti_type.(tup2 RepoModel.Address.t RepoModel.VerifiedAt.t)
  ;;

  let update : type a. Pool_common.Database.Label.t -> a t -> unit Lwt.t =
   fun db_pool model ->
    let pool = Pool_common.Database.Label.value db_pool in
    match model with
    | Unverified { address; token; _ } ->
      Utils.Database.exec pool update_unverified_request (address, token)
    | Verified { address; verified_at; _ } ->
      Utils.Database.exec pool update_verified_request (address, verified_at)
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
           tup2 RepoModel.Address.t (tup2 RepoModel.Address.t RepoModel.Token.t))
  ;;

  let update_email db_pool old_email new_email =
    Utils.Database.exec
      (Pool_common.Database.Label.value db_pool)
      update_email_request
      (address old_email, (address new_email, token new_email))
  ;;

  let delete_request =
    {sql|
      DELETE FROM pool_email_verifications
      WHERE address = ?;
    |sql}
    |> Caqti_request.exec Caqti_type.string
  ;;

  let delete db_pool email =
    Utils.Database.exec
      (Pool_common.Database.Label.value db_pool)
      delete_request
    @@ Entity_email.address email
  ;;
end
