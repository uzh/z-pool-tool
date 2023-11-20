open Entity
module Id = Pool_common.Repo.Id

let t =
  let encode (m : t) = Ok (m.user, m.import_pending) in
  let decode (user, import_pending) =
    let open CCResult in
    Ok { user; import_pending }
  in
  Caqti_type.(
    custom ~encode ~decode Pool_user.Repo.(t2 user_caqti ImportPending.t))
;;

module Write = struct
  type t =
    { user_uuid : Pool_common.Id.t
    ; import_pending : Pool_user.ImportPending.t
    }

  let of_entity { Entity.user; import_pending } =
    let user_uuid = Pool_common.Id.of_string user.Sihl_user.id in
    { user_uuid; import_pending }
  ;;

  let t =
    let encode (m : t) = Ok (m.user_uuid, m.import_pending) in
    let decode _ =
      failwith
        Pool_common.(
          Message.WriteOnlyModel |> Utils.error_to_string Language.En)
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (t2 Pool_common.Repo.Id.t Pool_user.Repo.ImportPending.t))
  ;;
end
