open Entity
open Database.Caqti_encoders

let t =
  let decode
    (id, (version, (text, (published_at, (created_at, (updated_at, ()))))))
    =
    Ok { id; version; text; published_at; created_at; updated_at }
  in
  let encode _ = Pool_common.Utils.failwith Pool_message.Error.ReadOnlyModel in
  let open Schema in
  custom
    ~encode
    ~decode
    Caqti_type.
      [ Pool_common.Repo.Id.t
      ; string
      ; string
      ; option ptime
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
      Ok Data.[ m.id; m.version; m.text; m.published_at ]
    in
    let open Schema in
    custom
      ~encode
      ~decode
      Caqti_type.[ Pool_common.Repo.Id.t; string; string; option ptime ]
  ;;
end
