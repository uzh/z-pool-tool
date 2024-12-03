open Entity
open Database.Caqti_encoders

module Text = struct
  include Text

  let t =
    let encode = Pool_common.Repo.encode_yojson yojson_of_t in
    let decode =
      Pool_common.Repo.decode_yojson t_of_yojson Pool_message.Field.Name
    in
    Caqti_type.(custom ~encode ~decode string)
  ;;
end

let t =
  let decode
        ( id
        , ( text
          , ( start_at
            , ( end_at
              , ( show_to_admins
                , (show_to_contacts, (created_at, (updated_at, ()))) ) ) ) ) )
    =
    Ok
      { id
      ; text
      ; start_at
      ; end_at
      ; show_to_admins
      ; show_to_contacts
      ; created_at
      ; updated_at
      }
  in
  let encode _ = Pool_common.Utils.failwith Pool_message.Error.ReadOnlyModel in
  let open Schema in
  custom
    ~encode
    ~decode
    Caqti_type.
      [ Pool_common.Repo.Id.t
      ; Text.t
      ; option ptime
      ; option ptime
      ; bool
      ; bool
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
      Ok
        Data.
          [ m.id
          ; m.text
          ; m.start_at
          ; m.end_at
          ; m.show_to_admins
          ; m.show_to_contacts
          ]
    in
    let open Schema in
    custom
      ~encode
      ~decode
      Caqti_type.
        [ Pool_common.Repo.Id.t
        ; Text.t
        ; option ptime
        ; option ptime
        ; bool
        ; bool
        ]
  ;;
end
