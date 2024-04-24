module Common = Pool_common.Repo
open Entity

module AdminComment = struct
  include AdminComment

  let t = Caqti_type.string
end

let t =
  let encode _ =
    Pool_message.Error.ReadOnlyModel |> Pool_common.Utils.failwith
  in
  let decode
    (id, (contact, (experiment, (admin_comment, (created_at, updated_at)))))
    =
    Ok { id; contact; experiment; admin_comment; created_at; updated_at }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (t2
         Common.Id.t
         (t2
            Contact.Repo.Entity.t
            (t2
               Experiment.Repo.Entity.t
               (t2
                  (option AdminComment.t)
                  (t2 Common.CreatedAt.t Common.UpdatedAt.t))))))
;;

module Write = struct
  type t =
    { id : Entity.Id.t
    ; contact_id : Pool_user.Id.t
    ; experiment_id : Experiment.Id.t
    ; admin_comment : Entity.AdminComment.t option
    }
  [@@deriving eq, show]

  let of_entity { id; contact; experiment; admin_comment; _ } =
    { id
    ; contact_id = Contact.id contact
    ; experiment_id = experiment.Experiment.id
    ; admin_comment
    }
  ;;

  let create ?(id = Entity.Id.create ()) contact_id experiment_id admin_comment =
    { id; contact_id; experiment_id; admin_comment }
  ;;

  let t =
    let encode (m : t) =
      Ok (m.id, (m.contact_id, (m.experiment_id, m.admin_comment)))
    in
    let decode _ =
      Pool_message.Error.WriteOnlyModel |> Pool_common.Utils.failwith
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (t2
           Pool_common.Repo.Id.t
           (t2
              Pool_user.Repo.Id.t
              (t2 Experiment.Repo.Entity.Id.t (option AdminComment.t)))))
  ;;
end
