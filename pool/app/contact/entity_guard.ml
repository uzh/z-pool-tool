module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    Guard.Persistence.Target.decorate
      ?ctx
      (fun user ->
        Guard.Target.make
          `Contact
          (user |> Entity.id |> Guard.Uuid.target_of Pool_common.Id.value))
      t
    |> Lwt_result.map_error Pool_common.Message.authorization
  ;;
end
