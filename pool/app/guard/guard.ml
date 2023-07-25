include Core
include Event
module Persistence = Repo
module Act = ActorSpec
module Tar = TargetSpec

(** [console_authorizable] is an Persistence.Role.t Actor.t] for use in
    administrative tasks, such as working with the command line or running
    tests. *)
let console_authorizable : Persistence.Role.t Actor.t =
  Actor.make (RoleSet.singleton `System) `System (Uuid.Actor.create ())
;;

(** [guest_authorizable] is a [Persistence.Role.t Actor.t] to be assigned to
    entities at the absolute lowest level of trust, such as users browsing the
    public facing website without logging in. *)
let guest_authorizable : Persistence.Role.t Actor.t =
  Actor.make (RoleSet.singleton `Guest) `Guest (Uuid.Actor.create ())
;;

(** The list of permissions that we need [Guardian] to be aware of in order to
    achieve a minimal level of functionality. Notably, the [`Admin] role should
    have [Manage] authority on everything in the system. *)
let root_permissions : Rule.t list =
  let open Core.Action in
  [ Act.Entity `LocationManagerAll, Manage, Tar.Entity `Location
  ; Act.Entity `RecruiterAll, Manage, Tar.Entity `Assignment
  ; Act.Entity `RecruiterAll, Manage, Tar.Entity `Contact
  ; Act.Entity `RecruiterAll, Manage, Tar.Entity `CustomField
  ; Act.Entity `RecruiterAll, Manage, Tar.Entity `CustomFieldGroup
  ; Act.Entity `RecruiterAll, Manage, Tar.Entity `Experiment
  ; Act.Entity `RecruiterAll, Manage, Tar.Entity `Filter
  ; Act.Entity `RecruiterAll, Manage, Tar.Entity `I18n
  ; Act.Entity `RecruiterAll, Manage, Tar.Entity `Invitation
  ; Act.Entity `RecruiterAll, Manage, Tar.Entity `Mailing
  ; Act.Entity `RecruiterAll, Manage, Tar.Entity `Session
  ; Act.Entity `RecruiterAll, Manage, Tar.Entity `Tag
  ]
  |> fun (init : Rule.t list) ->
  CCList.fold_product
    (fun acc actor role -> acc @ [ Act.Entity actor, Manage, Tar.Entity role ])
    init
    [ `Operator; `System; `Root ]
    Role.Target.all
;;

module Access = struct
  let manage_rules = ValidationSet.SpecificRole `ManageRules
end
