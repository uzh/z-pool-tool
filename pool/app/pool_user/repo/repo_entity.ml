open CCFun

let make_caqti_type = Pool_common.Repo.make_caqti_type

module Id = struct
  include Entity.Id

  let t = make_caqti_type Caqti_type.string (of_string %> CCResult.return) value
end

module Status = Pool_common.Repo.Model.SelectorType (Entity.Status)

module Paused = struct
  include Entity.Paused

  let t = Caqti_type.bool
end

module Disabled = struct
  include Entity.Disabled

  let t = Caqti_type.bool
end

module TermsAccepted = struct
  include Entity.TermsAccepted

  let t = make_caqti_type Caqti_type.ptime (create %> CCResult.return) value
end

module Verified = struct
  include Entity.Verified

  let t = make_caqti_type Caqti_type.ptime (create %> CCResult.return) value
end

module ImportPending = struct
  include Entity.ImportPending

  let t = Caqti_type.bool
end

module CellPhone = struct
  include Entity.CellPhone

  let t = Caqti_type.string
end

module UnverifiedCellPhone = struct
  include Entity.UnverifiedCellPhone

  let t =
    let encode (m : t) = Ok (m.cell_phone, m.created_at) in
    let decode (cell_phone, created_at) = Ok { cell_phone; created_at } in
    Caqti_type.(custom ~encode ~decode (t2 CellPhone.t Pool_common.Repo.CreatedAt.t))
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
        (t3 CellPhone.t Pool_common.Repo.VerificationCode.t Pool_common.Repo.CreatedAt.t))
  ;;
end

module EmailAddress = struct
  include Entity.EmailAddress

  let t =
    make_caqti_type
      Caqti_type.string
      (fun email ->
         email
         |> create
         |> CCResult.map_err
              (const Pool_message.(Error.InvalidWithInfo (Field.EmailAddress, email))))
      value
  ;;
end

module EmailVerified = struct
  include Entity.EmailVerified

  let t = make_caqti_type Caqti_type.ptime (create %> CCResult.return) value
end

module Firstname = struct
  include Entity.Firstname

  let t = Caqti_type.string
end

module Lastname = struct
  include Entity.Lastname

  let t = Caqti_type.string
end

module IsAdmin = struct
  include Entity.IsAdmin

  let t = Caqti_type.bool
end

module Confirmed = struct
  include Entity.Confirmed

  let t = Caqti_type.bool
end

let t =
  let open Database.Caqti_encoders in
  let decode (id, (email, (lastname, (firstname, (status, (admin, (confirmed, ()))))))) =
    Ok { Entity.id; email; lastname; firstname; status; admin; confirmed }
  in
  let encode (m : Entity.t) : ('a Data.t, string) result =
    Ok
      Data.
        [ m.Entity.id
        ; m.Entity.email
        ; m.Entity.lastname
        ; m.Entity.firstname
        ; m.Entity.status
        ; m.Entity.admin
        ; m.Entity.confirmed
        ]
  in
  custom
    ~encode
    ~decode
    Schema.
      [ Id.t; EmailAddress.t; Lastname.t; Firstname.t; Status.t; IsAdmin.t; Confirmed.t ]
;;
