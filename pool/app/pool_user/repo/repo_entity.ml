open CCFun

module Id = struct
  include Entity.Id

  let t =
    Pool_common.Repo.make_caqti_type
      Caqti_type.string
      (of_string %> CCResult.return)
      value
  ;;
end

module Status = Pool_common.Repo.Model.SelectorType (Entity.Status)

module Paused = struct
  include Entity.Paused

  let t =
    Pool_common.Repo.make_caqti_type
      Caqti_type.bool
      (create %> CCResult.return)
      value
  ;;
end

module Disabled = struct
  include Entity.Disabled

  let t =
    Pool_common.Repo.make_caqti_type
      Caqti_type.bool
      (create %> CCResult.return)
      value
  ;;
end

module TermsAccepted = struct
  include Entity.TermsAccepted

  let t =
    Pool_common.Repo.make_caqti_type
      Caqti_type.ptime
      (create %> CCResult.return)
      value
  ;;
end

module Verified = struct
  include Entity.Verified

  let t =
    Pool_common.Repo.make_caqti_type
      Caqti_type.ptime
      (create %> CCResult.return)
      value
  ;;
end

module CellPhone = struct
  include Entity.CellPhone

  let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
end

module ImportPending = struct
  include Entity.ImportPending

  let t =
    Pool_common.Repo.make_caqti_type
      Caqti_type.bool
      (create %> CCResult.return)
      value
  ;;
end

module UnverifiedCellPhone = struct
  include Entity.UnverifiedCellPhone

  let t =
    let encode (m : t) = Ok (m.cell_phone, m.created_at) in
    let decode (cell_phone, created_at) = Ok { cell_phone; created_at } in
    Caqti_type.(
      custom ~encode ~decode (t2 CellPhone.t Pool_common.Repo.CreatedAt.t))
  ;;

  let full =
    let encode _ = failwith "Decode model only." in
    let decode (cell_phone, verification_code, created_at) =
      Ok { cell_phone; verification_code; created_at }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (t3
           CellPhone.t
           Pool_common.Repo.VerificationCode.t
           Pool_common.Repo.CreatedAt.t))
  ;;
end

module HashedPassword = struct
  include Entity.HashedPassword

  let t =
    Pool_common.Repo.make_caqti_type
      Caqti_type.string
      (of_string %> CCResult.return)
      value
  ;;
end

module EmailAddress = struct
  include Entity.EmailAddress

  let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
end

module EmailVerified = struct
  include Entity.EmailVerified

  let t =
    Pool_common.Repo.make_caqti_type
      Caqti_type.ptime
      (create %> CCResult.return)
      value
  ;;
end

module Firstname = struct
  include Entity.Firstname

  let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
end

module Lastname = struct
  include Entity.Lastname

  let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
end

let t =
  let open Database.Caqti_encoders in
  let decode
    ( id
    , ( email
      , ( name
        , ( given_name
          , ( password
            , (status, (admin, (confirmed, (created_at, (updated_at, ()))))) )
          ) ) ) )
    =
    Ok
      { Entity.id
      ; email
      ; name
      ; given_name
      ; password
      ; status
      ; admin
      ; confirmed
      ; created_at
      ; updated_at
      }
  in
  let encode (m : Entity.t) : ('a Data.t, string) result =
    Ok
      Data.
        [ m.Entity.id
        ; m.Entity.email
        ; m.Entity.name
        ; m.Entity.given_name
        ; m.Entity.password
        ; m.Entity.status
        ; m.Entity.admin
        ; m.Entity.confirmed
        ; m.Entity.created_at
        ; m.Entity.updated_at
        ]
  in
  custom
    ~encode
    ~decode
    Schema.
      [ Id.t
      ; EmailAddress.t
      ; Lastname.t
      ; Firstname.t
      ; HashedPassword.t
      ; Status.t
      ; Caqti_type.bool
      ; Caqti_type.bool
      ; Pool_common.Repo.CreatedAt.t
      ; Pool_common.Repo.UpdatedAt.t
      ]
;;
