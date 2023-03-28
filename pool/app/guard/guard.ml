Logs.Src.create "guard"

include Core
include Event
module Act = ActorSpec
module Tar = TargetSpec

module Persistence = struct
  (* TODO: Once the guardian package has a cached version, this implementation
     can be updated/removed (Issue:
     https://github.com/uzh/guardian/issues/11) *)
  include Repo

  module Cache = struct
    open CCCache

    let equal_find_actor (l1, r1, a1) (l2, r2, a2) =
      Pool_database.Label.equal l1 l2
      && Role.equal r1 r2
      && Uuid.Actor.equal a1 a2
    ;;

    let equal_validation (l1, s1, a1) (l2, s2, a2) =
      Pool_database.Label.equal l1 l2
      && ValidationSet.equal s1 s2
      && Core.Actor.(Uuid.Actor.equal (id a1) (id a2))
    ;;

    let lru_find_actor = lru ~eq:equal_find_actor 2048
    let lru_validation = lru ~eq:equal_validation 2048

    let clear () =
      let () = clear lru_validation in
      clear lru_find_actor
    ;;
  end

  module Actor = struct
    include Actor

    let find database_label typ id =
      let cb ~in_cache _ _ =
        if in_cache
        then (
          let tags = Pool_database.Logger.Tags.create database_label in
          Logs.debug ~src (fun m ->
            m ~tags "Found in cache: Actor %s" (id |> Uuid.Actor.to_string)))
        else ()
      in
      let find' (label, typ, id) =
        find ~ctx:(Pool_database.to_ctx label) typ id
      in
      (database_label, typ, id)
      |> CCCache.(with_cache ~cb Cache.lru_find_actor find')
    ;;
  end

  let validate database_label validation_set (actor : Role.t Core.Actor.t) =
    let cb ~in_cache _ _ =
      if in_cache
      then
        Logs.debug ~src (fun m ->
          m
            ~tags:(Pool_database.Logger.Tags.create database_label)
            "Found in cache: Actor %s\nValidation set %s"
            (actor |> Core.Actor.id |> Uuid.Actor.to_string)
            ([%show: ValidationSet.t] validation_set))
      else ()
    in
    let validate' (label, set, actor) =
      validate
        ~ctx:(Pool_database.to_ctx label)
        Pool_common.Message.authorization
        set
        actor
    in
    (database_label, validation_set, actor)
    |> CCCache.(with_cache ~cb Cache.lru_validation validate')
  ;;
end

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
  ]
  |> fun (init : Rule.t list) ->
  CCList.fold_product
    (fun acc actor role -> acc @ [ Act.Entity actor, Manage, Tar.Entity role ])
    init
    [ `OperatorAll; `System; `Root ]
    Role.Target.all_entities
;;

module Utils = struct
  include Guardian.Utils

  let create_simple_dependency_with_pool
    kind
    parent_kind
    fcn_res
    kind_id_encode
    parent_id_decode
    ?ctx
    (action, spec)
    =
    let open Utils.Lwt_result.Infix in
    let pool = ctx |> CCFun.flip CCOption.bind Pool_database.of_ctx_opt in
    match[@warning "-4"] pool, spec with
    | Some pool, TargetSpec.Id (typ, id) when typ = kind ->
      let id = id |> Uuid.Target.to_string |> kind_id_encode in
      fcn_res pool id
      ||> (function
            | Ok id ->
              let id = Uuid.target_of parent_id_decode id in
              Some (action, TargetSpec.Id (parent_kind, id))
            | Error _ -> None)
      |> Lwt_result.ok
    | (_, TargetSpec.Id (typ, _) | _, TargetSpec.Entity typ) when typ = kind ->
      Some (action, TargetSpec.Entity parent_kind) |> Lwt.return_ok
    | _ -> Lwt.return_error "Invalid entity provided"
  ;;
end
