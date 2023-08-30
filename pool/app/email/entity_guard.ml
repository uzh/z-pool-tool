open Utils.Lwt_result.Infix

module SmtpTarget = struct
  type t = Entity.SmtpAuth.t

  let to_authorizable ?ctx t =
    let open Guard in
    Persistence.Target.decorate
      ?ctx
      (fun ({ Entity.SmtpAuth.id; _ } : Entity.SmtpAuth.t) ->
        Target.create `Smtp (id |> Uuid.target_of Pool_common.Id.value))
      t
    >|- Pool_common.Message.authorization
  ;;
end

module Access = struct
  open Guard
  open ValidationSet
  open Permission

  module Smtp = struct
    let smtp action id =
      (action, id |> Uuid.target_of Entity.SmtpAuth.Id.value |> TargetEntity.id)
      |> one
    ;;

    let index = One (Read, TargetEntity.Model `Smtp)
    let create = One (Create, TargetEntity.Model `Smtp)
    let read = smtp Read
    let update = smtp Update
    let delete = smtp Delete
  end
end
