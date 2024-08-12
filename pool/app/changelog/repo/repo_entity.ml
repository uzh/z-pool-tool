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
  let encode _ = failwith "Read only model" in
  let decode
    ( id
    , ( model
      , (entity_uuid, (user_uuid, (user_email, (changes, (created_at, ()))))) )
    )
    =
    Ok { id; model; entity_uuid; user_uuid; user_email; changes; created_at }
  in
  custom
    ~encode
    ~decode
    Schema.
      [ RepoId.t
      ; Field.t
      ; RepoId.t
      ; RepoId.t
      ; Pool_user.Repo.EmailAddress.t
      ; Changes.t
      ; Pool_common.Repo.CreatedAt.t
      ]
;;

module Write = struct
  let t =
    let open Database.Caqti_encoders in
    let encode m : ('a Data.t, string) result =
      let open Write in
      Ok
        Data.
          [ m.Write.id
          ; m.model
          ; m.entity_uuid
          ; m.user_uuid
          ; m.changes
          ; m.created_at
          ]
    in
    let decode _ = failwith "Write only model" in
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
end
