open Entity
open CCFun

module Paused = struct
  include Paused

  let t =
    Pool_common.Repo.make_caqti_type
      Caqti_type.bool
      (create %> CCResult.return)
      value
  ;;
end

module Disabled = struct
  include Disabled

  let t =
    Pool_common.Repo.make_caqti_type
      Caqti_type.bool
      (create %> CCResult.return)
      value
  ;;
end

module TermsAccepted = struct
  include TermsAccepted

  let t =
    Pool_common.Repo.make_caqti_type
      Caqti_type.ptime
      (create %> CCResult.return)
      value
  ;;
end

module Verified = struct
  include Verified

  let t =
    Pool_common.Repo.make_caqti_type
      Caqti_type.ptime
      (create %> CCResult.return)
      value
  ;;
end

module CellPhone = struct
  include CellPhone

  let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
end

module ImportPending = struct
  include ImportPending

  let t =
    Pool_common.Repo.make_caqti_type
      Caqti_type.bool
      (create %> CCResult.return)
      value
  ;;
end

module UnverifiedCellPhone = struct
  include UnverifiedCellPhone

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

module EmailAddress = struct
  include EmailAddress

  let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
end

module EmailVerified = struct
  include EmailVerified

  let t =
    Pool_common.Repo.make_caqti_type
      Caqti_type.ptime
      (create %> CCResult.return)
      value
  ;;
end

module User = struct
  let status =
    let encode m = m |> Sihl_user.status_to_string |> CCResult.return in
    let decode = Sihl_user.status_of_string in
    Caqti_type.(custom ~encode ~decode string)
  ;;

  let t =
    let open Database.Caqti_encoders in
    let open Sihl.Contract.User in
    let decode
      ( id
      , ( email
        , ( username
          , ( name
            , ( given_name
              , ( password
                , (status, (admin, (confirmed, (created_at, (updated_at, ())))))
                ) ) ) ) ) )
      =
      Ok
        { id
        ; email
        ; username
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
    let encode m : ('a Data.t, string) result =
      Ok
        Data.
          [ m.id
          ; m.email
          ; m.username
          ; m.name
          ; m.given_name
          ; m.password
          ; m.status
          ; m.admin
          ; m.confirmed
          ; m.created_at
          ; m.updated_at
          ]
    in
    custom
      ~encode
      ~decode
      Schema.(
        Caqti_type.
          [ string
          ; string
          ; option string
          ; option string
          ; option string
          ; string
          ; status
          ; bool
          ; bool
          ; ptime
          ; ptime
          ])
  ;;
end
