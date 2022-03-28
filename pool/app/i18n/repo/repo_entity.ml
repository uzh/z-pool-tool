open Entity
module Common = Pool_common

module Key = struct
  include Key

  let t = Caqti_type.string
end

module Content = struct
  include Content

  let t = Caqti_type.string
end

let t =
  let encode m =
    Ok (Common.Id.value m.id, (Key.to_string m.key, (m.language, m.content)))
  in
  let decode (id, (key, (language, content))) =
    let open CCResult in
    map_err (fun _ ->
        Pool_common.(
          Utils.error_to_string
            Pool_common.Language.En
            (Message.Decode Message.I18n)))
    @@ let* key = Key.of_string key in
       let* content = Content.create content in
       Ok { id = Common.Id.of_string id; key; language; content }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2
         Common.Repo.Id.t
         (tup2 Key.t (tup2 Pool_common.Language.t Content.t))))
;;
