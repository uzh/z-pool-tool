open Entity
module Id = Pool_common.Id
module RepoId = Pool_common.Repo.Id

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
    Ok (Id.value m.id, (m.room, (m.building, (m.street, (m.zip, m.city)))))
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
