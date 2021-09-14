open Entity

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
  let encode m =
    Ok
      ( Common.Id.show m.id
      , ( Title.show m.title
        , (Description.show m.description, (m.created_at, m.updated_at)) ) )
  in
  let decode (id, (title, (description, (created_at, updated_at)))) =
    let ( let* ) = Result.bind in
    let* title = Title.create title in
    let* description = Description.create description in
    Ok
      { id = Common.Id.of_string id
      ; title
      ; description
      ; created_at
      ; updated_at
      }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2
         Common.Repo.Id.t
         (tup2 Title.t (tup2 Description.t (tup2 ptime ptime)))))
;;

let find_by_id = Utils.todo
let list_all = Utils.todo
let insert = Utils.todo
let update = Utils.todo
let destroy = Utils.todo
