include Entity.Database

module Url = struct
  include Url

  let t = Caqti_type.string
end

module User = struct
  include User

  let t = Caqti_type.string
end

let t =
  let encode m = Ok (Url.value m.Entity.Database.url, User.value m.user) in
  let decode (url, user) =
    let ( let* ) = Result.bind in
    let* url = Url.create url in
    let* user = User.create user in
    Ok { url; user }
  in
  Caqti_type.(custom ~encode ~decode (tup2 Url.t User.t))
;;
