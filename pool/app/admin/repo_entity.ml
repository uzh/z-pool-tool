open Entity

let t =
  let encode (m : t) = Ok m.user in
  let decode user =
    let open CCResult in
    Ok { user }
  in
  Caqti_type.(custom ~encode ~decode Pool_user.Repo.user_caqti)
;;
