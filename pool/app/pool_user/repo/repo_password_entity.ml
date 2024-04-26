open CCFun.Infix
include Entity_password

let t =
  Pool_common.Repo.make_caqti_type
    Caqti_type.string
    (of_hash %> CCResult.return)
    value
;;
