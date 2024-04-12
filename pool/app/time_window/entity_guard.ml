open Utils.Lwt_result.Infix

module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    let open Guard in
    let open Entity in
    Persistence.Target.decorate
      ?ctx
      (fun ({ id; _ } : t) ->
        Target.create
          `Session
          (id |> Session.Id.to_common |> Uuid.target_of Pool_common.Id.value))
      t
    >|- Pool_common.Message.authorization
  ;;
end
