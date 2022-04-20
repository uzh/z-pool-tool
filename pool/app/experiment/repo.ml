open Entity
module Id = Pool_common.Id
module RepoId = Pool_common.Repo.Id

module Title = struct
  include Title

  let t = Caqti_type.string
end

module Description = struct
  include Description

  let t = Caqti_type.string
end

module ExperimentDate = struct
  include ExperimentDate

  let t = Caqti_type.ptime
end

let t =
  let encode (m : t) =
    Ok
      ( Id.value m.id
      , ( Title.value m.title
        , ( Description.value m.description
          , (m.filter, (m.created_at, m.updated_at)) ) ) )
  in
  let decode (id, (title, (description, (filter, (created_at, updated_at))))) =
    let open CCResult in
    let* title = Title.create title in
    let* description = Description.create description in
    Ok
      { id = Id.of_string id
      ; title
      ; description
      ; filter
      ; created_at
      ; updated_at
      }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2
         RepoId.t
         (tup2 Title.t (tup2 Description.t (tup2 string (tup2 ptime ptime))))))
;;

let find _ = Utils.todo
let list_all _ = Utils.todo
let insert _ = Utils.todo
let update _ = Utils.todo
let destroy _ = Utils.todo
