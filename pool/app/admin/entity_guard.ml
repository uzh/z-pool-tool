module Actor = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    Guard.Persistence.Actor.decorate
      ?ctx
      (fun t ->
        Guard.Actor.make
          (Guard.RoleSet.singleton `Admin)
          `Admin
          (t
           |> Entity.user
           |> fun Sihl_user.{ id; _ } -> id |> Guard.Uuid.Actor.of_string_exn))
      t
    |> Lwt_result.map_error Pool_common.Message.authorization
  ;;
end

module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    Guard.Persistence.Target.decorate
      ?ctx
      (fun t ->
        Guard.Target.make
          `Admin
          (t
           |> Entity.user
           |> fun Sihl_user.{ id; _ } -> id |> Guard.Uuid.Target.of_string_exn))
      t
    |> Lwt_result.map_error Pool_common.Message.authorization
  ;;
end

module ActorRole = struct
  let location_manager = function
    | None -> `LocationManagerAll
    | Some id ->
      let target_id = Guard.Uuid.target_of Pool_common.Id.value id in
      `LocationManager target_id
  ;;
end

module RuleSet = struct
  open Guard
  open Action
  module Act = ActorSpec
  module Tar = TargetSpec

  let assistant id =
    let target_id = Uuid.target_of Entity.Id.value id in
    let actor = Act.Entity (`Assistant target_id) in
    [ actor, Read, Tar.Id (`Experiment, target_id)
    ; ( actor
      , Read
      , Tar.Entity `Experiment (* TODO: Remove once index pages are filtered *)
      )
    ; actor, Read, Tar.Entity `Location
    ]
  ;;

  let experimenter id =
    let target_id = Uuid.target_of Entity.Id.value id in
    let actor = Act.Entity (`Experimenter target_id) in
    [ actor, Update, Tar.Id (`Experiment, target_id)
    ; ( actor
      , Read
      , Tar.Entity `Experiment (* TODO: Remove once index pages are filtered *)
      )
    ; actor, Read, Tar.Entity `Location
    ]
  ;;

  let location_manager id =
    let target_id = Uuid.target_of Entity.Id.value id in
    let actor = Act.Entity (`LocationManager target_id) in
    [ actor, Manage, Tar.Id (`Location, target_id)
    ; actor, Read, Tar.Entity `Location
    ; actor, Manage, Tar.Entity `LocationFile
    ]
  ;;
end
