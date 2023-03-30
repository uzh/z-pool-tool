open CCFun.Infix
open Utils.Lwt_result.Infix
open Guard

let target_of = Uuid.target_of Entity.Id.value

module Actor = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    Persistence.Actor.decorate
      ?ctx
      (Entity.user
       %> (fun { Sihl_user.id; _ } -> id |> Uuid.Actor.of_string_exn)
       %> Actor.make (RoleSet.singleton `Admin) `Admin)
      t
    >|- Pool_common.Message.authorization
  ;;
end

module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    Persistence.Target.decorate
      ?ctx
      (Entity.user
       %> (fun { Sihl_user.id; _ } -> id |> Uuid.Target.of_string_exn)
       %> Target.make `Admin)
      t
    >|- Pool_common.Message.authorization
  ;;
end

module ActorRole = struct
  let location_manager = function
    | None -> `LocationManagerAll
    | Some id -> `LocationManager (target_of id)
  ;;
end

module RuleSet = struct
  open Action
  module Act = ActorSpec
  module Tar = TargetSpec

  let assistant id =
    let actor = Act.Entity (`Assistant (target_of id)) in
    [ actor, Read, Tar.Id (`Experiment, target_of id)
    ; actor, Read, Tar.Entity `Location
    ]
  ;;

  let experimenter id =
    let actor = Act.Entity (`Experimenter (target_of id)) in
    [ actor, Update, Tar.Id (`Experiment, target_of id)
    ; actor, Read, Tar.Entity `Location
    ]
  ;;

  let location_manager id =
    let actor = Act.Entity (`LocationManager (target_of id)) in
    [ actor, Manage, Tar.Id (`Location, target_of id) ]
  ;;
end

module Access = struct
  open ValidationSet

  let index = One (Action.Read, TargetSpec.Entity `Admin)
  let read id = One (Action.Read, TargetSpec.Id (`Admin, target_of id))
  let update id = One (Action.Update, TargetSpec.Id (`Admin, target_of id))
end
