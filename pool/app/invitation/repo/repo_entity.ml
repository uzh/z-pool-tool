open Entity

module ResentAt = struct
  include Entity.ResentAt

  let t = Caqti_type.ptime
end

module SendCount = struct
  include Entity.SendCount

  let t = Pool_common.Repo.make_caqti_type Caqti_type.int create value
end

let t =
  let encode _ = failwith "Read model only" in
  let decode (id, (contact, (resent_at, (send_count, (created_at, updated_at)))))
    =
    let open CCResult in
    Ok { id; contact; resent_at; send_count; created_at; updated_at }
  in
  let open Pool_common.Repo in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (t2
         Id.t
         (t2
            Contact.Repo.Entity.t
            (t2
               (option ResentAt.t)
               (t2 SendCount.t (t2 CreatedAt.t UpdatedAt.t))))))
;;

module Update = struct
  type t =
    { id : Pool_common.Id.t
    ; resent_at : Entity.ResentAt.t option
    ; send_count : Entity.SendCount.t
    }

  let t =
    let encode (m : Entity.t) = Ok Entity.(m.id, (m.resent_at, m.send_count)) in
    let decode _ = failwith "Write model only" in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (t2 Pool_common.Repo.Id.t (t2 (option ResentAt.t) SendCount.t)))
  ;;
end

module Write = struct
  type t =
    { id : Pool_common.Id.t
    ; experiment_id : Experiment.Id.t
    ; contact_id : Pool_common.Id.t
    ; resent_at : Entity.ResentAt.t option
    ; send_count : SendCount.t
    ; created_at : Pool_common.CreatedAt.t
    ; updated_at : Pool_common.UpdatedAt.t
    }

  let of_entity (experiment_id : Experiment.Id.t) (m : Entity.t) : t =
    { id = m.Entity.id
    ; experiment_id
    ; contact_id = Contact.id m.Entity.contact
    ; resent_at = m.Entity.resent_at
    ; send_count = m.Entity.send_count
    ; created_at = m.Entity.created_at
    ; updated_at = m.Entity.updated_at
    }
  ;;

  let t =
    let encode (m : t) =
      Ok
        ( m.id
        , ( m.experiment_id
          , ( m.contact_id
            , (m.resent_at, (m.send_count, (m.created_at, m.updated_at))) ) ) )
    in
    let decode _ = failwith "Write model only" in
    let open Pool_common.Repo in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (t2
           Id.t
           (t2
              Experiment.Repo.Entity.Id.t
              (t2
                 Id.t
                 (t2
                    (option ResentAt.t)
                    (t2 SendCount.t (t2 CreatedAt.t UpdatedAt.t)))))))
  ;;
end
