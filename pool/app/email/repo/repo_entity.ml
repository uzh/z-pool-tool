open Entity
module User = Common_user

module Token = struct
  include User.Email.Token

  let t = Caqti_type.(string)
end

module Address = struct
  include User.Email.Address

  let t = Caqti_type.(string)
end

module VerifiedAt = struct
  include User.Email.VerifiedAt

  let t = Caqti_type.(ptime)
end

let unverified_t =
  let open CCResult in
  let encode (Unverified m) =
    Ok
      ( m.address |> User.Email.Address.value
      , m.token |> User.Email.Token.value
      , m.created_at |> Pool_common.CreatedAt.value
      , m.updated_at |> Pool_common.UpdatedAt.value )
  in
  let decode (address, token, created_at, updated_at) =
    let* address =
      address |> User.Email.Address.create |> CCResult.map_err (fun _ -> "TODO")
    in
    let token = token |> User.Email.Token.create in
    Ok (Unverified { address; token; created_at; updated_at })
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup4
         Address.t
         Token.t
         Pool_common.Repo.CreatedAt.t
         Pool_common.Repo.UpdatedAt.t))
;;

let verified_t =
  let open CCResult in
  let encode (Verified m) =
    Ok
      ( m.address |> Common.Email.Address.value
      , m.verified_at |> Common.Email.VerifiedAt.value
      , m.created_at |> Pool_common.CreatedAt.value
      , m.updated_at |> Pool_common.UpdatedAt.value )
  in
  let decode (address, verified_at, created_at, updated_at) =
    (* TODO [timhub]: Fix map_err *)
    let* address =
      address
      |> Common.Email.Address.create
      |> CCResult.map_err (fun _ -> "TODO")
    in
    let verified_at = verified_at |> User.Email.VerifiedAt.create in
    Ok (Verified { address; verified_at; created_at; updated_at })
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup4
         Address.t
         VerifiedAt.t
         Pool_common.Repo.CreatedAt.t
         Pool_common.Repo.UpdatedAt.t))
;;
