open Entity
module User = Pool_user

module Token = struct
  include Entity.Token

  let t = Caqti_type.(string)
end

module VerifiedAt = struct
  include Entity.VerifiedAt

  let t = Caqti_type.(ptime)
end

let unverified_t =
  let open CCResult in
  let encode (Unverified m) =
    Ok
      ( m.address |> User.EmailAddress.value
      , ( m.user_id |> Pool_common.Id.value
        , ( m.token |> Token.value
          , ( m.created_at |> Pool_common.CreatedAt.value
            , m.updated_at |> Pool_common.UpdatedAt.value ) ) ) )
  in
  let decode (address, (user_id, (token, (created_at, updated_at)))) =
    map_err (fun _ -> "decode unverified email")
    @@ let* address = address |> User.EmailAddress.create in
       let user_id = user_id |> Pool_common.Id.of_string in
       let token = token |> Token.create in
       Ok (Unverified { address; user_id; token; created_at; updated_at })
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2
         User.Repo.EmailAddress.t
         (tup2
            Pool_common.Repo.Id.t
            (tup2
               Token.t
               (tup2 Pool_common.Repo.CreatedAt.t Pool_common.Repo.UpdatedAt.t)))))
;;

let verified_t =
  let open CCResult in
  let encode (Verified m) =
    Ok
      ( m.address |> User.EmailAddress.value
      , ( m.user_id |> Pool_common.Id.value
        , ( m.verified_at |> VerifiedAt.value
          , ( m.created_at |> Pool_common.CreatedAt.value
            , m.updated_at |> Pool_common.UpdatedAt.value ) ) ) )
  in
  let decode (address, (user_id, (verified_at, (created_at, updated_at)))) =
    map_err (fun _ -> "decode verified email")
    @@ let* address = address |> User.EmailAddress.create in
       let user_id = user_id |> Pool_common.Id.of_string in
       let verified_at = verified_at |> VerifiedAt.create in
       Ok (Verified { address; user_id; verified_at; created_at; updated_at })
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2
         User.Repo.EmailAddress.t
         (tup2
            Pool_common.Repo.Id.t
            (tup2
               VerifiedAt.t
               (tup2 Pool_common.Repo.CreatedAt.t Pool_common.Repo.UpdatedAt.t)))))
;;
