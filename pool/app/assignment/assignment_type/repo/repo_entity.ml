open Entity

let public_assignment =
  let encode (m : Entity.public_session) = Ok (m.assignment, m.session) in
  let decode (assignment, session) = Ok { assignment; session } in
  Caqti_type.(
    custom ~encode ~decode (tup2 Assignment.Repo.Public.t Session.Repo.Public.t))
;;
