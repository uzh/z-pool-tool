open Entity
module Common = Pool_common
module Key = Pool_common.Repo.Model.SelectorType (Key)

module Content = struct
  include Content

  let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
end

let t =
  let encode m = Ok (m.id, (m.key, (m.language, m.content))) in
  let decode (id, (key, (language, content))) = Ok { id; key; language; content } in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (t2 Common.Repo.Id.t (t2 Key.t (t2 Common.Repo.Language.t Content.t))))
;;

let t_with_default_content =
  let encode _ = failwith "Decode model only." in
  let decode (id, (key, (language, content))) =
    let open CCResult in
    let content = CCOption.value ~default:"" content |> Content.of_string in
    Ok { id; key; language; content }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (t2 Common.Repo.Id.t (t2 Key.t (t2 Common.Repo.Language.t (option string)))))
;;
