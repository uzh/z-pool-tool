open Entity_group
module Repo = Pool_common.Repo

let t =
  let encode (m : t) = Ok (m.id, (m.model, m.name)) in
  let decode (id, (model, name)) = Ok { id; model; name } in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2 Repo.Id.t (tup2 Repo_entity.Model.t Repo_entity.Name.t)))
;;
