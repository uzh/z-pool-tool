module Actor = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    Guard.Persistence.Actor.decorate
      ?ctx
      (fun t ->
        Guard.Authorizable.make
          (Guard.ActorRoleSet.singleton `Admin)
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

  let to_authorizable ?ctx role t =
    Guard.Persistence.Target.decorate
      ?ctx
      (fun t ->
        Guard.AuthorizableTarget.make
          (Guard.TargetRoleSet.singleton (`Admin role))
          (`Admin role)
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
  let assistant id =
    let target_id = Guard.Uuid.target_of Entity.Id.value id in
    [ `ActorEntity (`Assistant target_id), `Read, `Target target_id
    ; `ActorEntity (`Assistant target_id), `Read, `TargetEntity `Experiment
    ; `ActorEntity (`Assistant target_id), `Read, `TargetEntity `Session
    ; `ActorEntity (`Assistant target_id), `Read, `TargetEntity `Assignment
    ]
  ;;

  let experimenter id =
    let target_id = Guard.Uuid.target_of Entity.Id.value id in
    let actor = `ActorEntity (`Experimenter target_id) in
    [ actor, `Read, `TargetEntity `Experiment
    ; actor, `Update, `Target target_id
    ; actor, `Manage, `TargetEntity `Session
    ; actor, `Manage, `TargetEntity `Assignment
    ; actor, `Manage, `TargetEntity `WaitingList
    ; actor, `Read, `TargetEntity `Invitation
    ; actor, `Update, `TargetEntity `Invitation
    ; actor, `Read, `TargetEntity `Location
    ; actor, `Read, `TargetEntity `LocationFile
    ; actor, `Read, `TargetEntity `Mailing
    ]
  ;;

  let location_manager id =
    let target_id = Guard.Uuid.target_of Entity.Id.value id in
    let actor = `ActorEntity (`LocationManager target_id) in
    [ actor, `Manage, `Target target_id
    ; actor, `Create, `TargetEntity `LocationFile
    ; actor, `Read, `TargetEntity `LocationFile
    ; actor, `Update, `TargetEntity `LocationFile
    ]
  ;;
end
