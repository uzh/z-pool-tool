type t =
  { id : Pool_common.Id.t
  ; subject_id : Pool_common.Id.t
  ; experiment_id : Pool_common.Id.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }

let create ?(id = Pool_common.Id.create ()) subject_id experiment_id =
  { id
  ; subject_id
  ; experiment_id
  ; created_at = Pool_common.CreatedAt.create ()
  ; updated_at = Pool_common.UpdatedAt.create ()
  }
;;

let to_entity (m : t) subject experiment =
  Entity.
    { id = m.id
    ; subject
    ; experiment
    ; created_at = m.created_at
    ; updated_at = m.updated_at
    }
;;

let t =
  let encode m =
    Ok
      ( Pool_common.Id.value m.id
      , ( Pool_common.Id.value m.subject_id
        , (Pool_common.Id.value m.experiment_id, (m.created_at, m.updated_at))
        ) )
  in
  let decode (id, (subject_id, (experiment_id, (created_at, updated_at)))) =
    let open CCResult in
    Ok
      { id = Pool_common.Id.of_string id
      ; subject_id = Pool_common.Id.of_string subject_id
      ; experiment_id = Pool_common.Id.of_string experiment_id
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

module Experiment = struct
  open Entity.ListByExperiment

  let t =
    let encode (m : waiting_list_entry) =
      Ok (Pool_common.Id.value m.id, (m.subject, (m.created_at, m.updated_at)))
    in
    let decode (id, (subject, (created_at, updated_at))) =
      let open CCResult in
      Ok { id = Pool_common.Id.of_string id; subject; created_at; updated_at }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (tup2
           Pool_common.Repo.Id.t
           (tup2
              Subject.Repo.List.t
              (tup2 Pool_common.Repo.CreatedAt.t Pool_common.Repo.UpdatedAt.t))))
  ;;
end
