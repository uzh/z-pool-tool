open Entity
module User = Common_user

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
      , m.token |> Token.value
      , m.created_at |> Pool_common.CreatedAt.value
      , m.updated_at |> Pool_common.UpdatedAt.value )
  in
  let decode (address, token, created_at, updated_at) =
    map_err (fun _ -> "decode unverified email")
    @@ let* address = address |> User.EmailAddress.create in
       let token = token |> Token.create in
       Ok (Unverified { address; token; created_at; updated_at })
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup4
         User.Repo.EmailAddress.t
         Token.t
         Pool_common.Repo.CreatedAt.t
         Pool_common.Repo.UpdatedAt.t))
;;

let verified_t =
  let open CCResult in
  let encode (Verified m) =
    Ok
      ( m.address |> User.EmailAddress.value
      , m.verified_at |> VerifiedAt.value
      , m.created_at |> Pool_common.CreatedAt.value
      , m.updated_at |> Pool_common.UpdatedAt.value )
  in
  let decode (address, verified_at, created_at, updated_at) =
    map_err (fun _ -> "decode verified email")
    @@ let* address = address |> User.EmailAddress.create in
       let verified_at = verified_at |> VerifiedAt.create in
       Ok (Verified { address; verified_at; created_at; updated_at })
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup4
         User.Repo.EmailAddress.t
         VerifiedAt.t
         Pool_common.Repo.CreatedAt.t
         Pool_common.Repo.UpdatedAt.t))
;;
