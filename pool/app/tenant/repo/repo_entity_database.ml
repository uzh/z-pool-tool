include Entity.Database

module Url = struct
  include Url

  let t = Caqti_type.string
end

module Label = struct
  include Label

  let t = Caqti_type.string
end

let t =
  let encode m = Ok (m.Entity.Database.url, m.label) in
  let decode (url, user) =
    let ( let* ) = Result.bind in
    let* url = Url.create url in
    let* label = Label.create user in
    Ok { url; label }
  in
  Caqti_type.(custom ~encode ~decode (tup2 Url.t Label.t))
;;
