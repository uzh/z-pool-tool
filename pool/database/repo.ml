open CCFun
include Entity

let make_caqti_type caqti_type create value =
  let encode = value %> CCResult.return in
  let decode = create %> CCResult.map_err Pool_message.Error.show in
  Caqti_type.(custom ~encode ~decode caqti_type)
;;

module Label = struct
  include Label

  let t = make_caqti_type Caqti_type.string create value
end

module Url = struct
  include Url

  let t =
    let open Utils.Crypto.String in
    make_caqti_type
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
