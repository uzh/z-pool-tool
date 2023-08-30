module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    let open Utils.Lwt_result.Infix in
    Guard.Persistence.Target.decorate
      ?ctx
      (fun Entity.{ id; _ } ->
        Guard.Target.create
          `I18n
          (id |> Guard.Uuid.target_of Pool_common.Id.value))
      t
    >|- Pool_common.Message.authorization
  ;;
end

module Access = struct
  open Guard
  open ValidationSet
  open Permission

  let i18n action id =
    let target_id = id |> Uuid.target_of Pool_common.Id.value in
    One (action, TargetEntity.Id target_id)
  ;;

  let index = One (Read, TargetEntity.Model `I18n)
  let update = i18n Update
end
