type t =
  { id : Pool_common.Id.t
  ; experiment_id : Pool_common.Id.t
  ; subject_id : Pool_common.Id.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }

let to_entity (m : t) (subject : Subject.t) : Entity.t =
  Entity.
    { id = m.id; subject; created_at = m.created_at; updated_at = m.updated_at }
;;

let of_entity (experiment_id : Pool_common.Id.t) (m : Entity.t) : t =
  { id = m.Entity.id
  ; experiment_id
  ; subject_id = Subject.id m.Entity.subject
  ; created_at = m.Entity.created_at
  ; updated_at = m.Entity.updated_at
  }
;;

let t =
  let encode m =
    Ok
      ( Pool_common.Id.value m.id
      , ( Pool_common.Id.value m.experiment_id
        , (Pool_common.Id.value m.subject_id, (m.created_at, m.updated_at)) ) )
  in
  let decode (id, (experiment_id, (subject_id, (created_at, updated_at)))) =
    let open CCResult in
    Ok
      { id = Pool_common.Id.of_string id
      ; experiment_id = Pool_common.Id.of_string experiment_id
      ; subject_id = Pool_common.Id.of_string subject_id
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
            Pool_common.Repo.Id.t
            (tup2
               Pool_common.Repo.Id.t
               (tup2 Pool_common.Repo.CreatedAt.t Pool_common.Repo.UpdatedAt.t)))))
;;
