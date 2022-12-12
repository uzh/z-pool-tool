include Core
include Event
module Persistence = Repo

(** [console_authorizable] is an [\[ `System \] Authorizable.t] for use in
    administrative tasks, such as working with the command line or running
    tests. *)
let console_authorizable =
  Authorizable.(
    make (ActorRoleSet.singleton `System) `System (Uuid.Actor.create ()))
;;

(** [guest_authorizable] is a [\[ `Guest \] Authorizable.t] to be assigned to
    entities at the absolute lowest level of trust, such as users browsing the
    public facing website without logging in. *)
let guest_authorizable =
  Authorizable.(
    make (ActorRoleSet.singleton `Guest) `Guest (Uuid.Actor.create ()))
;;

(** The list of permissions that we need [Guardian] to be aware of in order to
    achieve a minimal level of functionality. Notably, the [`Admin] role should
    have [`Manage] authority on everything in the system. *)
let root_permissions : Authorizer.auth_rule list =
  [ `ActorEntity `LocationManagerAll, `Manage, `TargetEntity `Location
  ; `ActorEntity `LocationManagerAll, `Manage, `TargetEntity `LocationFile
  ; `ActorEntity `RecruiterAll, `Manage, `TargetEntity `Assignment
  ; `ActorEntity `RecruiterAll, `Manage, `TargetEntity `Contact
  ; `ActorEntity `RecruiterAll, `Manage, `TargetEntity `CustomField
  ; `ActorEntity `RecruiterAll, `Manage, `TargetEntity `Experiment
  ; `ActorEntity `RecruiterAll, `Manage, `TargetEntity `Filter
  ; `ActorEntity `RecruiterAll, `Manage, `TargetEntity `I18n
  ; `ActorEntity `RecruiterAll, `Manage, `TargetEntity `Invitation
  ; `ActorEntity `RecruiterAll, `Manage, `TargetEntity `Mailing
  ; `ActorEntity `RecruiterAll, `Manage, `TargetEntity `Session
  ]
  |> fun (init : Authorizer.auth_rule list) ->
  CCList.fold_product
    (fun acc actor role ->
      acc @ [ `ActorEntity actor, `Manage, `TargetEntity role ])
    init
    [ `OperatorAll; `System; `Root ]
    Role.Target.all_entities
;;
