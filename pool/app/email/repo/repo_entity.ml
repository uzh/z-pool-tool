open Entity
module SmtpAuth = Repo_entity_smtp_auth
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
      ( m.address
      , ( m.user
        , ( m.token |> Token.value
          , ( m.created_at |> Pool_common.CreatedAt.value
            , m.updated_at |> Pool_common.UpdatedAt.value ) ) ) )
  in
  let decode (address, (user, (token, (created_at, updated_at)))) =
    map_err (fun _ ->
      let open Pool_common in
      Utils.error_to_string
        Language.En
        Message.(Decode Field.EmailAddressUnverified))
    @@
    let token = token |> Token.create in
    Ok (Unverified { address; user; token; created_at; updated_at })
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2
         User.Repo.EmailAddress.t
         (tup2
            Pool_user.Repo.user_caqti
            (tup2
               Token.t
               (tup2 Pool_common.Repo.CreatedAt.t Pool_common.Repo.UpdatedAt.t)))))
;;

let verified_t =
  let open CCResult in
  let encode (Verified m) =
    Ok (m.address, (m.user, (m.verified_at, (m.created_at, m.updated_at))))
  in
  let decode (address, (user, (verified_at, (created_at, updated_at)))) =
    map_err (fun _ ->
      let open Pool_common in
      Utils.error_to_string
        Language.En
        Message.(Decode Field.EmailAddressVerified))
    @@
    let verified_at = verified_at |> VerifiedAt.create in
    Ok (Verified { address; user; verified_at; created_at; updated_at })
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2
         User.Repo.EmailAddress.t
         (tup2
            Pool_user.Repo.user_caqti
            (tup2
               VerifiedAt.t
               (tup2 Pool_common.Repo.CreatedAt.t Pool_common.Repo.UpdatedAt.t)))))
;;
