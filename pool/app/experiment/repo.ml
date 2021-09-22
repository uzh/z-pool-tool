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

module Location = struct
  include Location

  module Room = struct
    include Room

    let t = Caqti_type.string
  end

  module Building = struct
    include Building

    let t = Caqti_type.string
  end

  module Street = struct
    include Street

    let t = Caqti_type.string
  end

  module Zip = struct
    include Zip

    let t = Caqti_type.string
  end

  module City = struct
    include City

    let t = Caqti_type.string
  end

  let t =
    let encode m =
      Ok
        ( Id.show m.Location.id
        , (m.room, (m.building, (m.street, (m.zip, m.city)))) )
    in
    let decode (id, (room, (building, (street, (zip, city))))) =
      Ok { id = Id.of_string id; room; building; street; zip; city }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (tup2
           RepoId.t
           (tup2 Room.t (tup2 Building.t (tup2 Street.t (tup2 Zip.t City.t))))))
  ;;
end

let t =
  let encode m =
    Ok
      ( Id.show m.id
      , ( Title.show m.title
        , (Description.show m.description, (m.created_at, m.updated_at)) ) )
  in
  let decode (id, (title, (description, (created_at, updated_at)))) =
    let ( let* ) = Result.bind in
    let* title = Title.create title in
    let* description = Description.create description in
    Ok { id = Id.of_string id; title; description; created_at; updated_at }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2 RepoId.t (tup2 Title.t (tup2 Description.t (tup2 ptime ptime)))))
;;

let find_by_id = Utils.todo
let list_all = Utils.todo
let insert = Utils.todo
let update = Utils.todo
let destroy = Utils.todo
