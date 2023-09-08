open CCFun.Infix

module Id = struct
  include Pool_common.Repo.Id
end

module Name = struct
  include Entity.Name

  let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
end

module Description = struct
  include Entity.Description

  let t =
    let encode = yojson_of_t %> Yojson.Safe.to_string %> CCResult.return in
    let decode =
      read %> CCResult.map_err Pool_common.(Utils.error_to_string Language.En)
    in
    Caqti_type.(custom ~encode ~decode string)
  ;;
end

module Address = struct
  include Entity_address

  module Mail = struct
    include Mail

    module Institution = struct
      include Institution

      let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
    end

    module Room = struct
      include Room

      let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
    end

    module Building = struct
      include Building

      let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
    end

    module Street = struct
      include Street

      let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
    end

    module Zip = struct
      include Zip

      let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
    end

    module City = struct
      include City

      let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
    end

    let t =
      let encode m =
        Ok (m.institution, (m.room, (m.building, (m.street, (m.zip, m.city)))))
      in
      let decode (institution, (room, (building, (street, (zip, city))))) =
        Ok { institution; room; building; street; zip; city }
      in
      Caqti_type.(
        custom
          ~encode
          ~decode
          (tup2
             (option Institution.t)
             (tup2
                (option Room.t)
                (tup2 (option Building.t) (tup2 Street.t (tup2 Zip.t City.t))))))
    ;;
  end

  let t =
    let encode = function
      | Virtual -> Ok (true, None)
      | Physical address -> Ok (false, Some address)
    in
    let decode (is_virtual, address) =
      match is_virtual, address with
      | true, _ -> Ok Virtual
      | _, Some address -> Ok (Physical address)
      | false, None ->
        failwith
          "Location could be created without beeing virtual and without \
           address!"
    in
    Caqti_type.(custom ~encode ~decode (tup2 bool (option Mail.t)))
  ;;
end

module Link = struct
  include Entity.Link

  let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
end

module Status = Pool_common.Repo.Model.SelectorType (Entity.Status)

type t =
  { id : Pool_common.Id.t
  ; name : Name.t
  ; description : Description.t option
  ; address : Address.t
  ; link : Link.t option
  ; status : Status.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show]

let t =
  let encode m =
    Ok
      ( m.id
      , ( m.name
        , ( m.description
          , (m.address, (m.link, (m.status, (m.created_at, m.updated_at)))) ) )
      )
  in
  let decode
    ( id
    , ( name
      , (description, (address, (link, (status, (created_at, updated_at))))) )
    )
    =
    let open CCResult in
    Ok { id; name; description; address; link; status; created_at; updated_at }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2
         Pool_common.Repo.Id.t
         (tup2
            Name.t
            (tup2
               (option Description.t)
               (tup2
                  Address.t
                  (tup2
                     (option Link.t)
                     (tup2
                        Status.t
                        (tup2
                           Pool_common.Repo.CreatedAt.t
                           Pool_common.Repo.UpdatedAt.t))))))))
;;

let to_entity (m : t) files : Entity.t =
  { Entity.id = m.id
  ; name = m.name
  ; description = m.description
  ; address = m.address
  ; link = m.link
  ; status = m.status
  ; files
  ; created_at = m.created_at
  ; updated_at = m.updated_at
  }
;;

let of_entity (m : Entity.t) : t =
  { id = m.Entity.id
  ; name = m.Entity.name
  ; description = m.Entity.description
  ; address = m.Entity.address
  ; link = m.Entity.link
  ; status = m.Entity.status
  ; created_at = m.Entity.created_at
  ; updated_at = m.Entity.updated_at
  }
;;

module Update = struct
  type t =
    { id : Pool_common.Id.t
    ; name : Name.t
    ; description : Description.t
    ; address : Address.t
    ; link : Link.t
    }

  let t =
    let encode (m : Entity.t) =
      Ok Entity.(m.id, (m.name, (m.description, (m.address, m.link))))
    in
    let decode _ = failwith "Write model only" in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (tup2
           Pool_common.Repo.Id.t
           (tup2
              Name.t
              (tup2 (option Description.t) (tup2 Address.t (option Link.t))))))
  ;;
end
