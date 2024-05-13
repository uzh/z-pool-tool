open CCFun.Infix
open Entity

let make_caqti_type = Pool_common.Repo.make_caqti_type

module Id = struct
  include Id

  let t = make_caqti_type Caqti_type.string (of_string %> CCResult.return) value
end

let t =
  let encode (m : t) = Ok (m.user, (m.email_verified, m.import_pending)) in
  let decode (user, (email_verified, import_pending)) =
    let open CCResult in
    Ok { user; email_verified; import_pending }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      Pool_user.Repo.(t2 t (t2 (option EmailVerified.t) ImportPending.t)))
;;

module Write = struct
  type t =
    { user_uuid : Id.t
    ; email_verified : Pool_user.EmailVerified.t option
    ; import_pending : Pool_user.ImportPending.t
    }

  let of_entity { Entity.user; email_verified; import_pending } =
    let user_uuid = user.Pool_user.id |> Id.of_user in
    { user_uuid; email_verified; import_pending }
  ;;

  let t =
    let open Database.Caqti_encoders in
    let decode _ =
      Pool_common.Utils.failwith Pool_message.Error.WriteOnlyModel
    in
    let encode (m : t) : ('a Data.t, string) result =
      Ok Data.[ m.user_uuid; m.email_verified; m.import_pending ]
    in
    let open Schema in
    custom
      ~encode
      ~decode
      Caqti_type.
        [ Id.t
        ; option Pool_user.Repo.EmailVerified.t
        ; Pool_user.Repo.ImportPending.t
        ]
  ;;
end
