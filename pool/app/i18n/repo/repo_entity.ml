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

(* TODO [timhub]: Shoud we use serialized data instead of language column? *)
let t =
  let open Property in
  let encode m = Ok (Common.Id.value m.id, (m.key, (m.language, m.content))) in
  let decode (id, (key, (language, content))) =
    let ( let* ) = Result.bind in
    let* key = Key.create key in
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
