open Entity
module SmtpAuth = Repo_entity_smtp_auth

module Token = struct
  include Token

  let t =
    Pool_common.Repo.make_caqti_type
      Caqti_type.string
      CCFun.(create %> CCResult.return)
      value
  ;;
end

module VerifiedAt = struct
  include VerifiedAt

  let t =
    Pool_common.Repo.make_caqti_type
      Caqti_type.ptime
      CCFun.(create %> CCResult.return)
      value
  ;;
end

let unverified_t =
  let open CCResult in
  let encode (Unverified m) =
    Ok (m.address, (m.user, (m.token, (m.created_at, m.updated_at))))
  in
  let decode (address, (user, (token, (created_at, updated_at)))) =
    Ok (Unverified { address; user; token; created_at; updated_at })
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (t2
         User.Repo.EmailAddress.t
         (t2
            Pool_user.Repo.t
            (t2
               Token.t
               (t2 Pool_common.Repo.CreatedAt.t Pool_common.Repo.UpdatedAt.t)))))
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
        Pool_message.(Error.Decode Field.EmailAddressVerified))
    @@ Ok (Verified { address; user; verified_at; created_at; updated_at })
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (t2
         Pool_user.Repo.EmailAddress.t
         (t2
            Pool_user.Repo.t
            (t2
               VerifiedAt.t
               (t2 Pool_common.Repo.CreatedAt.t Pool_common.Repo.UpdatedAt.t)))))
;;
