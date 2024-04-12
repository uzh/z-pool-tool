open Entity
module Id = Pool_common.Repo.Id

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
      Pool_user.Repo.(
        t2 User.t (t2 (option Pool_user.Repo.EmailVerified.t) ImportPending.t)))
;;

module Write = struct
  type t =
    { user_uuid : Pool_common.Id.t
    ; email_verified : Pool_user.EmailVerified.t option
    ; import_pending : Pool_user.ImportPending.t
    }

  let of_entity { Entity.user; email_verified; import_pending } =
    let user_uuid = Pool_common.Id.of_string user.Sihl_user.id in
    { user_uuid; email_verified; import_pending }
  ;;

  let t =
    let encode (m : t) =
      Ok (m.user_uuid, (m.email_verified, m.import_pending))
    in
    let decode _ =
      Pool_message.Error.WriteOnlyModel |> Pool_common.Utils.failwith
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (t2
           Pool_common.Repo.Id.t
           (t2
              (option Pool_user.Repo.EmailVerified.t)
              Pool_user.Repo.ImportPending.t)))
  ;;
end
