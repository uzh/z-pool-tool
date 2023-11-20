open Entity

module AdminComment = struct
  include AdminComment

  let t = Caqti_type.string
end

type t =
  { id : Pool_common.Id.t
  ; contact_id : Pool_common.Id.t
  ; experiment_id : Experiment.Id.t
  ; admin_comment : AdminComment.t option
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show]

let create
  ?(id = Pool_common.Id.create ())
  contact_id
  experiment_id
  admin_comment
  =
  { id
  ; contact_id
  ; experiment_id
  ; admin_comment
  ; created_at = Pool_common.CreatedAt.create ()
  ; updated_at = Pool_common.UpdatedAt.create ()
  }
;;

let to_entity (m : t) contact experiment =
  Entity.
    { id = m.id
    ; contact
    ; experiment
    ; admin_comment = m.admin_comment
    ; created_at = m.created_at
    ; updated_at = m.updated_at
    }
;;

let t =
  let encode (m : t) =
    Ok
      ( m.id
      , ( m.contact_id
        , (m.experiment_id, (m.admin_comment, (m.created_at, m.updated_at))) )
      )
  in
  let decode
    ( id
    , (contact_id, (experiment_id, (admin_comment, (created_at, updated_at))))
    )
    =
    Ok
      { id
      ; contact_id
      ; experiment_id
      ; admin_comment = CCOption.map AdminComment.create admin_comment
      ; created_at
      ; updated_at
      }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (t2
         Pool_common.Repo.Id.t
         (t2
            Pool_common.Repo.Id.t
            (t2
               Experiment.Repo.Entity.Id.t
               (t2
                  (option AdminComment.t)
                  (t2 Pool_common.Repo.CreatedAt.t Pool_common.Repo.UpdatedAt.t))))))
;;

module Experiment = struct
  open Entity.ExperimentList

  let t =
    let encode (m : waiting_list_entry) =
      Ok (m.id, (m.contact, (m.admin_comment, (m.created_at, m.updated_at))))
    in
    let decode (id, (contact, (admin_comment, (created_at, updated_at)))) =
      Ok
        { id
        ; contact
        ; admin_comment = CCOption.map AdminComment.create admin_comment
        ; created_at
        ; updated_at
        }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (t2
           Pool_common.Repo.Id.t
           (t2
              Contact.Repo.Preview.t
              (t2
                 (option AdminComment.t)
                 (t2 Pool_common.Repo.CreatedAt.t Pool_common.Repo.UpdatedAt.t)))))
  ;;
end
