open Entity
open CCFun.Infix
module RepoId = Pool_common.Repo.Id

let make_type = Pool_common.Repo.make_caqti_type

module Changes = struct
  let t =
    let open Changes in
    make_type Caqti_type.string (of_string %> CCResult.return) to_string
  ;;
end

module Field = struct
  open Pool_message.Field

  let t = make_type Caqti_type.string (read %> CCResult.return) show
end

let t =
  let open Database.Caqti_encoders in
  let encode m : ('a Data.t, string) result =
    Ok
      Data.
        [ m.id; m.model; m.entity_uuid; m.user_uuid; m.changes; m.created_at ]
  in
  let decode
    (id, (model, (entity_uuid, (user_uuid, (changes, (created_at, ()))))))
    =
    Ok { id; model; entity_uuid; user_uuid; changes; created_at }
  in
  custom
    ~encode
    ~decode
    Schema.
      [ RepoId.t
      ; Field.t
      ; RepoId.t
      ; RepoId.t
      ; Changes.t
      ; Pool_common.Repo.CreatedAt.t
      ]
;;
