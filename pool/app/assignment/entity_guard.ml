module Target = struct
  let (_ : (unit, string) result) =
    let find_parent =
      Guard.Utils.create_simple_dependency_with_pool
        `Assignment
        `Session
        Repo.find_session_id
        Pool_common.Id.of_string
        Pool_common.Id.value
    in
    Guard.Persistence.Dependency.register `Assignment `Session find_parent
  ;;

  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    Guard.Persistence.Target.decorate
      ?ctx
      (fun t ->
        Guard.Target.make
          `Assignment
          (t.Entity.id
           |> Pool_common.Id.value
           |> Guard.Uuid.Target.of_string_exn))
      t
    |> Lwt_result.map_error Pool_common.Message.authorization
  ;;
end
