open CCFun.Infix

module Target = struct
  (* TODO: Make sure this works *)
  type t = Entity.Value.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    let open Utils.Lwt_result.Infix in
    Guard.Persistence.Target.decorate
      ?ctx
      (Guard.Uuid.target_of Pool_common.Id.value %> Guard.Target.create `SystemSetting)
      t
    >|- Pool_message.Error.authorization
  ;;
end

module Access = struct
  open Guard
  open ValidationSet
  open Permission

  let setting action = one_of_tuple (action, `SystemSetting, None)
  let index = setting Read
  let create = setting Create
  let read = setting Read
  let update = setting Update
  let delete = setting Delete
end
