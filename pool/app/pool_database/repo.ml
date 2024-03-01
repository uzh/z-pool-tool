open CCFun
include Entity

module Label = struct
  include Label

  let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
end

module Url = struct
  include Url

  let t =
    let open Utils.Crypto.String in
    let open Pool_common in
    Repo.make_caqti_type
      Caqti_type.string
      (decrypt_from_string
       %> CCResult.map_err (fun _ ->
         Pool_message.(Error.Decode Field.DatabaseUrl)))
      encrypt_to_string
  ;;
end

let t =
  let encode m = Ok (m.url, m.label) in
  let decode (url, label) = Ok { url; label } in
  Caqti_type.(custom ~encode ~decode (t2 Url.t Label.t))
;;
