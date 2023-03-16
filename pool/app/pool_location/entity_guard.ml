module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    Guard.Persistence.Target.decorate
      ?ctx
      (fun Entity.{ id; _ } ->
        Guard.Target.make
          `Location
          (id |> Entity.Id.value |> Guard.Uuid.Target.of_string_exn))
      t
    |> Lwt_result.map_error Pool_common.Message.authorization
  ;;
end

module FileTarget = struct
  type t = Entity.Mapping.file [@@deriving eq, show]

  let to_authorizable ?ctx t =
    Guard.Persistence.Target.decorate
      ?ctx
      (fun Entity.Mapping.{ id; _ } ->
        Guard.Target.make
          `LocationFile
          (id |> Entity.Mapping.Id.value |> Guard.Uuid.Target.of_string_exn))
      t
    |> Lwt_result.map_error Pool_common.Message.authorization
  ;;

  let to_authorizable_of_write ?ctx t =
    Guard.Persistence.Target.decorate
      ?ctx
      (fun Entity.Mapping.Write.{ id; _ } ->
        Guard.Target.make
          `LocationFile
          (id |> Entity.Mapping.Id.value |> Guard.Uuid.Target.of_string_exn))
      t
    |> Lwt_result.map_error Pool_common.Message.authorization
  ;;
end
