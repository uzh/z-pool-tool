module Name = struct
  include Entity.Name

  let t = Caqti_type.string
end

module Description = struct
  include Entity.Description

  let t = Caqti_type.string
end

module MailingAddress = struct
  include Entity.MailingAddress

  let t = Caqti_type.string
end

module Link = struct
  include Entity.Link

  let t = Caqti_type.string
end

module Status = struct
  include Entity.Status

  let t = Caqti_type.string
end

type t =
  { id : Pool_common.Id.t
  ; name : Name.t
  ; description : Description.t
  ; address : MailingAddress.t
  ; link : Link.t
  ; status : Status.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show]

let t =
  let encode m =
    Ok
      ( Pool_common.Id.value m.id
      , ( m.name
        , ( m.description
          , ( m.address
            , (m.link, (m.status |> Status.show, (m.created_at, m.updated_at)))
            ) ) ) )
  in
  let decode
      ( id
      , ( name
        , (description, (address, (link, (status, (created_at, updated_at)))))
        ) )
    =
    let open CCResult in
    Ok
      { id = Pool_common.Id.of_string id
      ; name
      ; description
      ; address
      ; link
      ; status = status |> Status.read
      ; created_at
      ; updated_at
      }
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
               Description.t
               (tup2
                  MailingAddress.t
                  (tup2
                     Link.t
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
    ; address : MailingAddress.t
    ; asset_id : Pool_common.Id.t
    ; link : Link.t
    }

  let t =
    let encode (m : Entity.t) =
      Ok
        Entity.(
          ( Pool_common.Id.value m.id
          , (m.name, (m.description, (m.address, m.link))) ))
    in
    let decode _ = failwith "Write model only" in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (tup2
           Pool_common.Repo.Id.t
           (tup2 Name.t (tup2 Description.t (tup2 MailingAddress.t Link.t)))))
  ;;
end
