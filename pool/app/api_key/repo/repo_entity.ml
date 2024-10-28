open CCFun.Infix
open Entity
open Database.Caqti_encoders
module Common = Pool_common.Repo

module Token = struct
  include Token

  let t =
    let open Utils.Crypto.String in
    Common.make_caqti_type
      Caqti_type.string
      (decrypt_from_string
       %> CCResult.map_err (fun _ -> Pool_message.(Error.Decode Field.Token)))
      encrypt_to_string
  ;;
end

let t =
  let decode (id, (name, (token, (expires_at, (created_at, (updated_at, ()))))))
    =
    Ok { id; name; token; expires_at; created_at; updated_at }
  in
  let encode _ = Pool_common.Utils.failwith Pool_message.Error.ReadOnlyModel in
  let open Schema in
  custom
    ~encode
    ~decode
    Caqti_type.
      [ Pool_common.Repo.Id.t
      ; string
      ; Token.t
      ; ptime
      ; Pool_common.Repo.CreatedAt.t
      ; Pool_common.Repo.UpdatedAt.t
      ]
;;

module Write = struct
  let t =
    let decode _ =
      Pool_common.Utils.failwith Pool_message.Error.WriteOnlyModel
    in
    let encode m : ('a Data.t, string) result =
      Ok Data.[ m.id; m.name; m.token; m.expires_at ]
    in
    let open Schema in
    custom
      ~encode
      ~decode
      Caqti_type.[ Pool_common.Repo.Id.t; string; Token.t; ptime ]
  ;;
end
